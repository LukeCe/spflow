#' @include class_virtual.R compact-purrr.R utils.R

#' An S4 class to represent information on the nodes of a network.
#'
#' @slot network_id A character that serves as identifier for the network
#' @slot count A numeric that indicates the number of nodes in the network
#' @slot node_data A data.frame that contains all information describing the nodes
#' @slot neighborhood A matrix that describes the neighborhood of the nodes
#'
#' @importClassesFrom Matrix Matrix
#' @family node_information network_data
setClass("node_information",
         slots = c(network_id   = "character",
                   count        = "maybe_numeric",
                   neighborhood = "maybe_Matrix",
                   node_data   = "maybe_data.frame"))

#' @keywords internal
#' @family node_information
setValidity("node_information", function(object) {

  dim_neighborhood <- dim(object@neighborhood)

  # neighborhood must be square matrix
  if (!has_equal_elements(dim_neighborhood)) {
    error_msg <- "The neighborhood matrix must be a square matrix!"
    return(error_msg)
  }

  # neighborhood must have same row number as node_data
  dim_node_data <- dim(object@node_data)
  node_count <- c(dim_node_data[1],dim_neighborhood)
  if (!has_equal_elements(node_count)) {
    error_msg <- "Row number of node_data does not match the dimensions of the neighborhood matrix!"
    return(error_msg)
  }

  # node count musst be equal to rownumber of node_data and neighborhood
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
#' @param node_data A data.frame that contains all information describing the nodes
#' @param neighborhood A matrix that describes the neighborhood of the nodes
#'
#' @family node_information
#'
#' @return The S4 class node_information
#' @export
node_information <- function(
  network_id,
  neighborhood = NULL,
  node_data = NULL
) {

  dim_neighborhood <- dim(neighborhood)
  dim_node_data <- dim(node_data)
  node_count <- c(dim_node_data[1],dim_neighborhood)

  nodes <- new(
    "node_information",
    network_id   = network_id,
    neighborhood = try_coercion(neighborhood,"Matrix"),
    count        = unique(node_count),
    node_data   = as.data.frame(node_data))

  if (validObject(nodes))
    return(nodes)
}
