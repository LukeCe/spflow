#' @include class_virtual.R compact-purrr.R utils.R

#' An S4 class to represent information on the nodes of a network.
#'
#' @slot network_id A character that serves as identifier for the network
#' @slot node_count A numeric that indicates the number of nodes in the network
#' @slot node_data A data.frame that contains all information describing the nodes
#' @slot node_neighborhood A matrix that describes the neighborhood of the nodes
#'
#' @importClassesFrom Matrix Matrix
#' @family sp_network sp_multi_network
#'
#' @export
setClass("sp_network",
         slots = c(
           network_id        = "character",
           node_count        = "maybe_numeric",
           node_neighborhood = "maybe_Matrix",
           node_data         = "maybe_data.table"))

# validity --------------------------------------------------------------------
setValidity(
  Class = "sp_network",
  method =  function(object) {

    dim_neighborhood <- dim(object@node_neighborhood)

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
    node_count <- c(node_count,object@node_count)
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
    return(object@node_count)
  })

setMethod(
  f = "dat",
  signature = "sp_network",
  definition = function(object) { # data ----
    return(object@node_data)
    })

setReplaceMethod(
  f = "dat",
  signature = "sp_network",
  definition = function(object,value) {
    object@node_data <- value

    # remove neighborhood if there is a mismatch in dimensions
    dim_net <- c(nrow(object@node_data),dim(object@node_neighborhood))
    if (!has_equal_elements(dim_net)) {
      object@node_neighborhood <- NULL
      object@node_count <- dim_net[1]
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
    return(object@node_neighborhood)
  })

setReplaceMethod(
  f = "neighborhood",
  signature = "sp_network",
  definition = function(object,value) {
    object@node_neighborhood <- value

    # remove node_data if there is a mismatch in dimensions
    dim_net <- c(dim(object@node_neighborhood),nrow(object@node_data))
    if (!has_equal_elements(dim_net)) {
      object@node_data <- NULL
      object@node_count <- dim_net[1]
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
#' @param node_neighborhood A matrix that describes the neighborhood of the nodes
#'
#' @family sp_network
#' @importFrom data.table :=
#'
#' @return The S4 class sp_network
#' @export
sp_network <- function(
  network_id,
  node_neighborhood = NULL,
  node_data = NULL,
  node_id_column = NULL
) {

  dim_neighborhood <- dim(node_neighborhood)
  dim_node_data <- dim(node_data)
  node_count <- c(dim_node_data[1],dim_neighborhood) %>% unique()

  nodes <- new(
    "sp_network",
    network_id        = network_id,
    node_neighborhood = try_coercion(node_neighborhood,"Matrix"),
    node_count        = node_count,
    node_data         = node_data %|!|% data.table::as.data.table(node_data))

  # determine the key used for sorting and merging
  data_needs_key <- is.null(node_id_column) & !is.null(node_data)
  data_has_key <-  !is.null(node_id_column) & !is.null(node_data)

  if (data_needs_key) {
    node_keys <- network_id %p% "_" %p% seq_len(nrow(node_data))
    nodes@node_data[, id := factor_in_order(node_keys)] %>%
      data.table::setkey(.,id)
  }

  if (data_has_key) {
    data.table::setnames(nodes@node_data, node_id_column, "id")
    nodes@node_data[, id := factor_in_order(id)] %>%
      data.table::setkey(.,id)
  }

  if (validObject(nodes))
    return(nodes)
}
