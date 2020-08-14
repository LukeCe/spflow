#' @title
#' An S4 class that holds on a single network
#'
#' @description
#' In this representation a network is composed of nodes which are must be
#' identified uniquely and can be described by variables stored in a data.frame.
#' The node neighborhood matrix describes strength of links between the nodes of
#' the network.
#'
#' @slot network_id A character that serves as identifier for the network
#' @slot node_count A numeric that indicates the number of nodes in the network
#' @slot node_data A data.frame that contains all information describing the nodes
#' @slot node_neighborhood A matrix that describes the neighborhood of the nodes
#'
#' @family sp_network
#'
#' @export
setClass("sp_network_nodes",
         slots = c(
           network_id        = "character",
           node_count        = "maybe_numeric",
           node_neighborhood = "maybe_Matrix",
           node_data         = "maybe_data.table"))

# ---- Methods ----------------------------------------------------------------

#' @export
#' @rdname count
setMethod(
  f = "count",
  signature = "sp_network_nodes",
  function(object) { # ---- count ---------------------------------------------
    return(object@node_count)
  })

#' @export
#' @rdname dat
setMethod(
  f = "dat",
  signature = "sp_network_nodes",
  function(object) { # ---- dat -----------------------------------------------
    return(object@node_data)
    })

#' @export
#' @rdname dat
setReplaceMethod(
  f = "dat",
  signature = "sp_network_nodes",
  function(object, value) {
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

#' @export
#' @rdname id
setMethod(
  f = "id",
  signature = "sp_network_nodes",
  function(object) { # ---- id ------------------------------------------------
    return(object@network_id)
  })

#' @export
#' @rdname id
setReplaceMethod(
  f = "id",
  signature = "sp_network_nodes",
  function(object, value) { # ---- id <- --------------------------------------
    object@network_id <- value
    if (validObject(object))
      return(object)
  })

#' @export
#' @rdname neighborhood
setMethod(
  f = "neighborhood",
  signature = "sp_network_nodes",
  function(object) { # ---- neighborhood --------------------------------------
    return(object@node_neighborhood)
  })

#' @export
#' @rdname neighborhood
setReplaceMethod(
  f = "neighborhood",
  signature = "sp_network_nodes",
  function(object,value) { # ---- neighborhood <- -----------------------------
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


#' @export
#' @rdname variable_names
setMethod(
  f = "variable_names",
  signature = "sp_network_nodes",
  function(object) { # ---- variable_names ------------------------------------
    return(names(object@node_data))
  })

#' @export
#' @rdname variable_names
setReplaceMethod(
  f = "variable_names",
  signature = "sp_network_nodes",
  function(object,value) { # ---- variable_names < ----------------------------
    names(object@node_data) <- value
    if (validObject(object))
      return(object)
  })

setValidity(
  Class = "sp_network_nodes",
  function(object) { # ---- validity ------------------------------------------


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

    # node count must be equal to row number of node_data and neighborhood
    node_count <- c(node_count,object@node_count)
    if (!has_equal_elements(node_count)) {
      error_msg <- "The node count is wrong!"
      return(error_msg)
    }
    TRUE
  })

# ---- Constructors -----------------------------------------------------------

#' Create an S4 object that contains information in the nodes of a network
#'
#' @param network_id A character that serves as identifier for the network
#' @param node_data A data.frame that contains all information describing the nodes
#' @param node_neighborhood A matrix that describes the neighborhood of the nodes
#' @param node_id_column A character indicating the column containing identifiers for the nodes
#'
#' @family sp_network
#' @importFrom data.table :=
#'
#' @return The S4 class sp_network_nodes
#' @export
sp_network_nodes <- function(
  network_id,
  node_neighborhood = NULL,
  node_data = NULL,
  node_id_column = NULL
) {

  dim_neighborhood <- dim(node_neighborhood)
  dim_node_data <- dim(node_data)
  node_count <- c(dim_node_data[1],dim_neighborhood) %>% unique()

  nodes <- new(
    "sp_network_nodes",
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
