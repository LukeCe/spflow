#' @include class_node-information.R class_od-pair-information.R class_virtual.R compact-purrr.R utils.R


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
#' @param ... objects of of type [node_information] and [od_pair_information]
#'
#' @return A S4 network data object
#' @export
network_data <- function(...) {

  information_list <- list(...)
  is_node_information <- rapply(information_list, is, class2 = "node_information")
  is_pair_information <- rapply(information_list, is, class2 = "od_pair_information")

  assert(
    length(information_list) == sum(is_node_information, is_pair_information),
    "All information that is not of type node_information or od_pair_information is discarded!",
    warn = TRUE
  )

  return(new("network_data",
             nodes = information_list[is_node_information],
             pairs = information_list[is_pair_information]))
}
