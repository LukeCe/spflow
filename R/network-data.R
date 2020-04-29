#' @include utils.R

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
#' @family node_information
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
