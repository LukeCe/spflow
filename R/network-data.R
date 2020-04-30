#' @include utils.R compact_purrr.R

# nodes -----------------------------------------------------------------------
# class unions to allow NULL in slots
lapply(c("data.frame","Matrix","numeric"), class_union_null)

#' An S4 class to represent information on the nodes of a network.
#'
#' @slot network_id A character that serves as identifier for the network
#' @slot count A numeric that indicates the number of nodes in the network
#' @slot attributes A data.frame that contains all information describing the nodes
#' @slot neighborhood A matrix that describes the neighborhood of the nodes
#'
#' @importClassesFrom Matrix Matrix
#' @family node_information network_data
setClass("node_information",
         slots = c(network_id   = "character",
                   count        = "maybe_numeric",
                   neighborhood = "maybe_Matrix",
                   attributes   = "maybe_data.frame"))

#' @keywords internal
#' @family node_information
setValidity("node_information", function(object) {

  dim_neighborhood <- dim(object@neighborhood)

  # neighborhood must be square matrix
  if (!has_equal_elements(dim_neighborhood)) {
    error_msg <- "The neighborhood matrix must be a square matrix!"
    return(error_msg)
  }

  # neighborhood must have same row number as attributes
  dim_attributes <- dim(object@attributes)
  node_count <- c(dim_attributes[1],dim_neighborhood)
  if (!has_equal_elements(node_count)) {
    error_msg <- "Row number of attributes does not match the dimensions of the neighborhood matrix!"
    return(error_msg)
  }

  # node count musst be equal to rownumber of attributes and neighborhood
  node_count <- c(node_count,object@count)
  if (!has_equal_elements(node_count)) {
    error_msg <- "The node count is wrong!"
    return(error_msg)
  }

  TRUE

})


#' Create an S4 object that contains information in the nodes of a network
#'
#' @param network_id A character that serves as identifier for the network
#' @param attributes A data.frame that contains all information describing the nodes
#' @param neighborhood A matrix that describes the neighborhood of the nodes
#'
#' @family node_information
#'
#' @return The S4 class node_information
#' @export
node_information <- function(
  network_id,
  neighborhood = NULL,
  attributes = NULL
  ) {

  dim_neighborhood <- dim(neighborhood)
  dim_attributes <- dim(attributes)
  node_count <- c(dim_attributes[1],dim_neighborhood)

  assert(dim_neighborhood %>% has_equal_elements(),
         "The neighborhood matrix must be a square matrix!")
  assert(node_count %>% has_equal_elements(),
         "Row number of attributes does not match the dimensions of the neighborhood matrix!")

  nodes <- new(
    "node_information",
    network_id   = network_id,
    neighborhood = try_coercion(neighborhood,"Matrix"),
    count        = unique(node_count),
    attributes   = try_coercion(attributes,"data.frame"))

  return(nodes)
}

# origin-destination-pairs ----------------------------------------------------
lapply(c("Matrix","numeric","list"), class_union_null)

#' An S4 class to represent information origin-destination pairs (od-pairs) composed of two nodes.
#'
#' Each origin destination pair is composed of two nodes (see [node_information()]).
#' All origins belong to the same (origin-) network and all destination belong to
#' the same (destination-) network.
#' It is possible to chose the same network for origins and destinations, which
#' enables to represent od-pairs within the same network.
#'
#' @slot origin_id A character that serves as identifier for the origin network
#' @slot origin_count A numeric that represents the number of nodes in the origin network
#' @slot destination_id A character that serves as identifier for the destination network
#' @slot destination_count A numeric that represents the number of nodes in the destination network
#' @slot pair_attributes A list of matrices that contain information on node pairs (origins are in rows and destinations are in columns)
#'
#' @family od_pair_information network_data
#' @importClassesFrom Matrix Matrix
#' @export
setClass("od_pair_information",
         slots = c(origin_id         = "character",
                   origin_count      = "maybe_numeric",
                   destination_id    = "character",
                   destination_count = "maybe_numeric",
                   pair_attributes   = "maybe_list"))


#' @keywords internal
#' @family od_pair_information
setValidity("od_pair_information", function(object) {

  consitent_od_dim <-
    lapply(object@pair_attributes, dim) %>%
    append(.,list(c(object@origin_count,
                    object@destination_count))) %>%
    has_equal_elements(.)

  if (!consitent_od_dim) {
    error_msg <- "The pair dimensions of the pair attributes imply an inconsitent number of origins and destinations!"
    return(error_msg)
  }

  TRUE
})


#' Create an S4 object that contains information on origin-destination pairs
#'
#' @param origin_id A character that serves as identifier for the origin network
#' @param destination_id A character that serves as identifier for the destination network
#' @param pair_attributes A list of matrices that contain information on node pairs (origins are in rows and destinations are in columns)
#'
#' @family od_pair_information network_data
#'
#' @return An S4 class of type
#' @export
od_pair_information <- function(
  origin_id,
  destination_id,
  pair_attributes = NULL
  ) {

  pair_attributes <- savely_to_list(pair_attributes)
  pair_attributes <- lapply(pair_attributes, try_coercion, "Matrix")

  pair_data_dimensions <-
    pair_attributes %>%
    lapply(., dim)

  assert(has_equal_elements(pair_data_dimensions),
         ("All supplied information on pairs of nodes must be " %p%
            "matrices with the same dimensions!"))

  pair_data_dimensions <- pair_data_dimensions %>% unique() %>% unlist()

  node_pair_data <- new(
    "od_pair_information",
    origin_id         = origin_id,
    origin_count      = pair_data_dimensions[1],
    destination_id    = destination_id,
    destination_count = pair_data_dimensions[2],
    pair_attributes   = pair_attributes
  )

  return(node_pair_data)
}


# multi network data ----------------------------------------------------------

#' An S4 class that contains [node_information] and [od_pair_information] for one or multiple networks
#'
#' @slot nodes A list of [node_information] objects
#' @slot pairs A list of [od_pair_information] objects
setClass("network_data",
         slots = c(nodes = "list",
                   pairs = "list"))


#' @keywords internal
#' @family network_data
#' @seealso node_information od_pair_information
setValidity("network_data", function(object) {

  valid_pairs <-
    c(rapply(object@pairs, is_one_of, .classes = c("od_pair_information","NULL")),
      rapply(object@pairs, validObject))

  if (!all(valid_pairs)) {
    error_msg <- "All objecets in the pairs accessors must be of class od_pair_information."
    return(error_msg)
  }

  valid_nodes <-
    c(rapply(object@nodes, is_one_of, .classes = c("node_information","NULL")),
      rapply(object@nodes, validObject))

  if (!all(valid_nodes)) {
    error_msg <- "All objecets in the nodes accessors must be of class node_information."
    return(error_msg)
  }

  network_names <- lapply(object@nodes, slot, name = "network_id")
  od_names <- lapply(object@pairs, pull_slots,.slots = c("origin_id","destination_id"))
  od_keys <- lapply(od_names, reduce, paste, sep = "_")

  if (!(has_distinct_elements(network_names)
        & has_distinct_elements(od_keys))
      ) {
    error_msg <- "The identification of all sets of nodes and od_pairs must be unique!"
    return(error_msg)
  }



  network_sizes <- lapply(object@nodes, slot, name = "count")
  names(network_sizes) <- network_names

  od_names <- reduce(od_names,c)
  od_sizes <-
    lapply(object@pairs, pull_slots,.slots = c("origin_count","destination_count")) %>%
    reduce(c) %>%
    setNames(.,od_names)

  consitent_node_numbers <-
    lapply(network_names,
           function(.name,.net,.od) c(.net[[.name]],.od[[.name]]),
           .net = network_sizes, .od = od_sizes) %>%
    rapply(., has_equal_elements)

  if (!all(consitent_node_numbers)) {
    error_msg <- "The number of nodes in network [%s] is not consitent!" %>%
      sprintf(.,paste(od_names[!consitent_node_numbers],sep = " and "))
    return(error_msg)
  }

  return(TRUE)
})


#' Create an S4 class that contains [node_information] and [od_pair_information] for one or multiple networks
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
network_data <- function(...) {

  information_list <- list(...)
  is_node_information <- rapply(information_list, is, class2 = "node_information")
  is_pair_information <- rapply(information_list, is, class2 = "od_pair_information")

  assert(
    length(information_list) == sum(is_node_information, is_pair_information),
    "All information that is not of type node_information or od_pair_information is discardes!",
    warn = TRUE
  )

  return(new("network_data",
             nodes = information_list[is_node_information],
             pairs = information_list[is_pair_information]))
}
