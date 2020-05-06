#' @include class_virtual.R compact-purrr.R utils.R

#' An S4 class to represent information on the nodes of a network.
#'
#' @slot network_id A character that serves as identifier for the network
#' @slot count A numeric that indicates the number of nodes in the network
#' @slot node_data A data.frame that contains all information describing the nodes
#' @slot neighborhood A matrix that describes the neighborhood of the nodes
#'
#' @importClassesFrom Matrix Matrix
#' @family sp_network sp_multi_network
setClass("sp_network",
         slots = c(
           network_id   = "character",
           count        = "maybe_numeric",
           neighborhood = "maybe_Matrix",
           node_data    = "maybe_data.frame"))

# validity --------------------------------------------------------------------
setValidity(
  Class = "sp_network",
  method =  function(object) {

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

# get and set -----------------------------------------------------------------
setMethod(
  f = "count",
  signature = "sp_network",
  definition = function(object) { # count ----
    return(object@count)
  })

setMethod(
  f = "data",
  signature = "sp_network",
  definition = function(object) { # data ----
    return(object@node_data)
    })

setReplaceMethod(
  f = "data",
  signature = "sp_network",
  definition = function(object,value) {
    object@node_data <- value

    # remove neighborhood if there is a mismatch in dimensions
    dim_net <- c(nrow(object@node_data),dim(object@neighborhood))
    if (!has_equal_elements(dim_net)) {
      object@neighborhood <- NULL
      object@count <- dim_net[1]
    }

    if (validObject(object)) {
      return(object)
    }
    })

setMethod(
  f = "id",
  signature = "sp_network",
  definition = function(object) { # id ----
    return(object@network_id)
  })

setReplaceMethod(
  f = "id",
  signature = "sp_network",
  definition = function(object,value) {
    object@network_id <- value
    if (validObject(object))
      return(object)
  })

setMethod(
  f = "neighborhood",
  signature = "sp_network",
  definition = function(object) { # neighborhood ----
    return(object@neighborhood)
  })

setReplaceMethod(
  f = "neighborhood",
  signature = "sp_network",
  definition = function(object,value) {
    object@neighborhood <- value

    # remove node_data if there is a mismatch in dimensions
    dim_net <- c(dim(object@neighborhood),nrow(object@node_data))
    if (!has_equal_elements(dim_net)) {
      object@node_data <- NULL
      object@count <- dim_net[1]
    }

    if (validObject(object))
      return(object)
  })

setMethod(
  f = "variable_names",
  signature = "sp_network",
  definition = function(object) { # variable_names ----
    return(names(object@node_data))
  })

setReplaceMethod(
  f = "variable_names",
  signature = "sp_network",
  definition = function(object,value) {
    names(object@node_data) <- value
    if (validObject(object))
      return(object)
  })

# constructors ----------------------------------------------------------------

#' Create an S4 object that contains information in the nodes of a network
#'
#' @param network_id A character that serves as identifier for the network
#' @param node_data A data.frame that contains all information describing the nodes
#' @param neighborhood A matrix that describes the neighborhood of the nodes
#'
#' @family sp_network
#'
#' @return The S4 class sp_network
#' @export
sp_network <- function(
  network_id,
  neighborhood = NULL,
  node_data = NULL
) {

  dim_neighborhood <- dim(neighborhood)
  dim_node_data <- dim(node_data)
  node_count <- c(dim_node_data[1],dim_neighborhood)

  nodes <- new(
    "sp_network",
    network_id   = network_id,
    neighborhood = try_coercion(neighborhood,"Matrix"),
    count        = unique(node_count),
    node_data    = node_data %|!|% as.data.frame(node_data))

  if (validObject(nodes))
    return(nodes)
}
