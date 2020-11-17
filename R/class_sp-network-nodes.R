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
#' @slot nnodes A numeric that indicates the number of nodes in the network
#' @slot node_data A data.frame that contains all information describing the nodes
#' @slot node_neighborhood A matrix that describes the neighborhood of the nodes
#'
#' @family sp_network
#'
#' @export
setClass("sp_network_nodes",
         slots = c(
           network_id        = "character",
           nnodes            = "maybe_numeric",
           node_neighborhood = "maybe_Matrix",
           node_data         = "maybe_data.table"))

# ---- Methods ----------------------------------------------------------------

#' @export
#' @rdname dat
setMethod(
  f = "dat",
  signature = "sp_network_nodes",
  function(object) { # ---- dat -----------------------------------------------
    return(object@node_data)
    })

#' @keywords internal
#' @rdname dat
setReplaceMethod(
  f = "dat",
  signature = "sp_network_nodes",
  function(object, value) { # ---- dat <- -------------------------------------
    object@node_data <- value

    # dimensions match ...
    no_confilcts <- is.null(value) || nrow(value) == object@nnodes
    if (no_confilcts && validObject(object))
      return(object)

    # dimensions don not match ...
    object@nnodes <- nrow(value)
    object@node_neighborhood <- NULL
    if (validObject(object))
      return(object)
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

#' @keywords internal
#' @rdname neighborhood
setReplaceMethod(
  f = "neighborhood",
  signature = "sp_network_nodes",
  function(object,value) { # ---- neighborhood <- -----------------------------
    object@node_neighborhood <- value %|!|% as(value,"Matrix")

    # dimensions match ...
    no_confilcts <- is.null(value) || nrow(value) == object@nnodes
    if (no_confilcts && validObject(object))
      return(object)

    # dimensions don not match ...
    object@nnodes <- nrow(value)
    object@node_data <- NULL
    if (validObject(object))
      return(object)
  })

#' @export
#' @rdname nnodes
setMethod(
  f = "nnodes",
  signature = "sp_network_nodes",
  function(object) { # ---- nnodes --------------------------------------------
    return(object@nnodes)
  })


#' @export
setMethod(
  f = "show",
  signature = "sp_network_nodes",
  function(object){ # ---- show -----------------------------------------------

    cat("Spatial network nodes with id:",id(object))
    cat("\n")
    cat(print_line(50))

    has_count <- !is.null(nnodes(object))
    if (has_count) {
      cat("\nNumber of nodes:", nnodes(object))
    }

    has_neighborhood <- !is.null(neighborhood(object))
    if (has_neighborhood) {
      cat("\nAverage number of links per node:",
          round(neighborhood(object) %>% nnzero()/
                  (neighborhood(object) %>% nrow()),3)
      )
      cat("\nDensity of the neighborhood matrix:",
          format_percent(neighborhood(object) %>% nnzero()/
                           neighborhood(object) %>% length()),
          "(non-zero connections)"
          )
    }

    has_data <- !is.null(dat(object))
    if (has_data) {
      cat("\n\nData on individual nodes:\n")
      print(dat(object))
    }
    cat("\n")
    invisible(object)
  })

setValidity(
  Class = "sp_network_nodes",
  function(object) { # ---- validity ------------------------------------------

    # check the id
    if (!is_single_character(id(object))) {
      error_msg <- "The network id is invalid!"
      return(error_msg)
    }

    # check dimensions of nb matrix
    dim_nb <- dim(neighborhood(object))
    consitent <- dim_nb %>% has_equal_elements()
    if (!consitent) {
      error_msg <- "The neighborhood matrix must be a square matrix!"
      return(error_msg)
    }

    # check dimensions of data and matrix
    nr_dat <- nrow(dat(object))
    nnodes <- nnodes(object)
    consitent <- c(nr_dat,dim_nb,nnodes) %>% has_equal_elements()
    if (!consitent) {
      error_msg <-
        "The row number of node_data does not match the dimensions of" %p%
        "the neighborhood matrix!"
      return(error_msg)
    }

    # check details of the data
    if (is.null(dat(object)))
      return(TRUE)

    data_id_col <- key(dat(object))
    if (is.null(data_id_col)) {
      error_msg <- "The data musst have an id column!"
      return(error_msg)
    }

    node_ids <- dat(object)[[data_id_col]]
    duplicated_ids <- length(unique(node_ids)) != nr_dat
    wrong_id_type <- !is.factor(node_ids)
    if (duplicated_ids | wrong_id_type ) {
      error_msg <- "The nodes are not correctly identifyed!"
      return(error_msg)
    }

    # object is valid
    return(TRUE)
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
#' @importFrom data.table := as.data.table setkey setnames
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
  nnodes <- c(dim_node_data[1],dim_neighborhood) %>% unique() %[[% 1

  nodes <- new(
    "sp_network_nodes",
    network_id        = network_id,
    node_neighborhood = try_coercion(node_neighborhood,"Matrix"),
    nnodes            = nnodes,
    node_data         = NULL)

  if (is.null(node_data))
    return(nodes)

  # determine the key used for sorting and merging
  assert_is_one_of(node_data, c("matrix", "data.frame","data.table"))
  node_data <- as.data.table(node_data)

  # Create the ID column and make it a key
  data_needs_key <- is.null(node_id_column)
  if (data_needs_key) {
    node_keys <- network_id %p% "_" %p% seq_len(nrow(node_data))
    node_data[, ID := factor_in_order(node_keys)]
  }
  if (!data_needs_key) {
    data.table::setnames(node_data, node_id_column, "ID")
    node_data[, ID := factor_in_order(ID)]
  }
  nodes@node_data <- node_data %>% setkey(ID)

  if (validObject(nodes))
    return(nodes)
}
