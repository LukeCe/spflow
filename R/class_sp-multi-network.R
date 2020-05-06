#' @include class_sp-network.R class_sp-network-pair.R class_virtual.R compact-purrr.R utils.R


#' An S4 class that contains [sp_network] and [sp_network_pair] for one or multiple networks
#'
#' @slot nodes A list of [sp_network] objects
#' @slot pairs A list of [sp_network_pair] objects
#'
#' @export
setClass("sp_multi_network",
         slots = c(nodes = "list",
                   pairs = "list"))

# validity --------------------------------------------------------------------
setValidity("sp_multi_network", function(object) {

  valid_pairs <-
    c(rapply(object@pairs, is_one_of, .classes = c("sp_network_pair","NULL")),
      rapply(object@pairs, validObject))

  if (!all(valid_pairs)) {
    error_msg <- "All objecets in the pairs accessors must be of class sp_network_pair."
    return(error_msg)
  }

  valid_nodes <-
    c(rapply(object@nodes, is_one_of, .classes = c("sp_network","NULL")),
      rapply(object@nodes, validObject))

  if (!all(valid_nodes)) {
    error_msg <- "All objecets in the nodes accessors must be of class sp_network."
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

# get and set -----------------------------------------------------------------


# methods ---------------------------------------------------------------------
setMethod(
  f = "interaction_data",
  signature = "sp_multi_network",
  definition = function(object, pair_id, interaction_id) { # data ----


    origin_id <- strsplit(pair_id,"_")[[1]][1]
    destination_id <- strsplit(pair_id,"_")[[1]][2]




    return(object@node_data)
  })

# constructors ----------------------------------------------------------------

#' Create an S4 class that contains [sp_network] and [sp_network_pair] for one or multiple networks
#'
#' @param ... objects of of type [sp_network] and [sp_network_pair]
#'
#' @return A S4 network data object
#' @export
sp_multi_network <- function(...) {

  information_list <- list(...)
  is_sp_network <- rapply(information_list, is, class2 = "sp_network")
  is_pair_information <- rapply(information_list, is, class2 = "sp_network_pair")

  assert(
    length(information_list) == sum(is_sp_network, is_pair_information),
    "All information that is not of type sp_network or sp_network_pair is discarded!",
    warn = TRUE
  )

  return(new("sp_multi_network",
             nodes = information_list[is_sp_network],
             pairs = information_list[is_pair_information]))
}


