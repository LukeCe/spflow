#' @include utils.R

# nodes -----------------------------------------------------------------------
# class unions to allow NULL in slots
lapply(c("data.frame","Matrix","numeric"), class_union_null)

#' An S4 class to represent information on the nodes of a network.
#'
#' @slot network_id A character that serves as identifier for the network
#' @slot count A numeric that indicates the number of nodes in the network
#' @slot attributes A data.frame that contains all information describing the nodes
#' @slot neighbourhood A matrix that describes the neighborhood of the nodes
#'
#' @importClassesFrom Matrix Matrix
#' @family node_information
setClass("node_information",
         slots = c(network_id      = "character",
                   count           = "maybe_numeric",
                   neighbourhood   = "maybe_Matrix",
                   attributes      = "maybe_data.frame"))

#' @keywords internal
#' @family node_information
setValidity("node_information", function(object) {

  dim_neighbourhood <- dim(object@neighbourhood)

  # neighbourhood must be square matrix
  if (!has_equal_elements(dim_neighbourhood)) {
    error_msg <- "The neighbourhood matrix must be a square matrix!"
    return(error_msg)
  }

  # neighbourhood must have same row number as attributes
  dim_attributes <- dim(object@attributes)
  node_count <- c(dim_attributes[1],dim_neighbourhood)
  if (!has_equal_elements(node_count)) {
    error_msg <- "Row number of attributes does not match the dimensions of the neighbourhood matrix!"
    return(error_msg)
  }

  # node count musst be equal to rownumber of attributes and neighbourhood
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
#' @param neighbourhood A matrix that describes the neighborhood of the nodes
#'
#' @family node_information
#'
#' @return The S4 class node_information
#' @export
node_information <- function(
  network_id,
  neighbourhood = NULL,
  attributes = NULL
  ) {

  dim_neighbourhood <- dim(neighbourhood)
  dim_attributes <- dim(attributes)
  node_count <- c(dim_attributes[1],dim_neighbourhood)

  assert(dim_neighbourhood %>% has_equal_elements(),
         "The neighbourhood matrix must be a square matrix!")
  assert(node_count %>% has_equal_elements(),
         "Row number of attributes does not match the dimensions of the neighbourhood matrix!")

  nodes <- new(
    "node_information",
    network_id      = network_id,
    neighbourhood   = try_coercion(neighbourhood,"Matrix"),
    count           = unique(node_count),
    attributes      = try_coercion(attributes,"data.frame"))

  return(nodes)
}
