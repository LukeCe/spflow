#' @title sp_network_nodes Class
#'
#' @description
#' An S4 class that contains all information on a single network.
#' In this representation a network is composed of nodes which are must be
#' identified uniquely by and ID.
#' Each node is described by variables stored in a data.frame.
#' The node neighborhood matrix describes strength of links between the nodes
#' of the network.
#' The class is constructed by the [sp_network_nodes()] function.
#'
#' @slot network_id
#'   A character that serves as identifier for the network
#' @slot nnodes
#'   A numeric that indicates the number of nodes in the network
#' @slot node_data
#'   A data.frame that contains all information describing the nodes
#' @slot node_neighborhood
#'   A matrix that describes the neighborhood relations of the nodes
#'
#' @family spflow network objects
#' @name sp_network_nodes-class
#' @export
setClass("sp_network_nodes",
         slots = c(
           network_id        = "character",
           nnodes            = "maybe_numeric",
           node_neighborhood = "maybe_any_matrix",
           node_data         = "maybe_data.frame"))

# ---- Methods ----------------------------------------------------------------

#' @rdname sp_network_nodes-class
#' @export
#' @examples
#' ## access the data describing the nodes
#' dat(germany_net)
#'
setMethod(
  f = "dat",
  signature = "sp_network_nodes",
  function(object) { # ---- dat -----------------------------------------------
    return(object@node_data)
    })

#' @rdname sp_network_nodes-class
#' @keywords internal
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
#' @rdname sp_network_nodes-class
#' @examples
#' # access the id of the network
#' germany_net2 <- germany_net
#' id(germany_net2)
#' id(germany_net2) <- "Germany"
#'
setMethod(
  f = "id",
  signature = "sp_network_nodes",
  function(object) { # ---- id ------------------------------------------------
    return(object@network_id)
  })

#' @rdname sp_network_nodes-class
#' @export
setReplaceMethod(
  f = "id",
  signature = "sp_network_nodes",
  function(object, value) { # ---- id <- --------------------------------------
    object@network_id <- value
    if (validObject(object))
      return(object)
  })

#' @rdname sp_network_nodes-class
#' @export
#' @examples
#' # access the neighborhood matrix of the nodes
#' neighborhood(germany_net)
#'
setMethod(
  f = "neighborhood",
  signature = "sp_network_nodes",
  function(object) { # ---- neighborhood --------------------------------------
    return(object@node_neighborhood)
  })

#' @rdname sp_network_nodes-class
#' @keywords internal
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

#' @rdname sp_network_nodes-class
#' @export
#' @examples
#' # access the number of nodes inside the network
#' nnodes(germany_net)
#'
setMethod(
  f = "nnodes",
  signature = "sp_network_nodes",
  function(object) { # ---- nnodes --------------------------------------------
    return(object@nnodes)
  })


#' @keywords internal
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
      nb_links <- nnzero(neighborhood(object))
      cat("\nAverage number of links per node:",
          round(nb_links/nnodes(object),3)
          )
      cat("\nDensity of the neighborhood matrix:",
          format_percent(nb_links/(nnodes(object)^2)),
          "(non-zero connections)"
          )
    }

    has_data <- !is.null(dat(object))
    if (has_data) {
      cat("\n\nData on nodes:\n")
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
    consitent <- has_equal_elements(dim_nb)
    if (!consitent) {
      error_msg <- "The neighborhood matrix must be a square matrix!"
      return(error_msg)
    }

    # check dimensions of data and matrix
    nr_dat <- nrow(dat(object))
    nnodes <- nnodes(object)
    consitent <- has_equal_elements(c(nr_dat,dim_nb,nnodes))
    if (!consitent) {
      error_msg <-
        "The row number of node_data does not match the dimensions of" %p%
        "the neighborhood matrix!"
      return(error_msg)
    }

    # check details of the data
    if (is.null(dat(object)))
      return(TRUE)


    data_id_col <- key(dat(object)) # TODO remove data.table dependency
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

#' Create a [sp_network_nodes-class()]
#'
#' @param network_id
#'   A character that serves as identifier for the network
#' @param node_data
#'   A data.frame that contains all information describing the nodes
#' @param node_neighborhood
#'   A matrix that describes the neighborhood of the nodes
#' @param node_id_column
#'   A character indicating the column containing identifiers for the nodes
#'
#' @family spflow network objects
#' @importFrom data.table := as.data.table setkey setnames
#'
#' @return The S4 class sp_network_nodes
#' @export
#' @examples
#' sp_network_nodes("germany",
#'                  spdep::nb2mat(spdep::poly2nb(germany_grid)),
#'                  as.data.frame(germany_grid),
#'                  "NOM")
sp_network_nodes <- function(
  network_id,
  node_neighborhood = NULL,
  node_data = NULL,
  node_id_column = NULL
) {

  dim_neighborhood <- dim(node_neighborhood)
  dim_node_data <- dim(node_data)
  nnodes <- c(dim_node_data[1],dim_neighborhood)
  nnodes <- unique(nnodes)[[1]]
  node_neighborhood <- node_neighborhood %|!|%
    try_coercion(node_neighborhood,"Matrix")

  nodes <- new(
    "sp_network_nodes",
    network_id        = network_id,
    node_neighborhood = node_neighborhood,
    nnodes            = nnodes,
    node_data         = NULL)

  if (is.null(node_data))
    return(nodes)

  # determine the key used for sorting and merging
  assert_is_one_of(node_data, c("matrix", "data.frame","data.table", "Matrix"))
  node_data <- as.data.table(node_data)

  # Create the ID column and make it a key
  data_needs_key <- is.null(node_id_column)

  # TODO remove data.table
  if (data_needs_key) {
    node_keys <- network_id %p% "_" %p% seq_len(nrow(node_data))
    node_data[, ID := factor_in_order(node_keys)]
  }
  if (!data_needs_key) {
    data.table::setnames(node_data, node_id_column, "ID")
    node_data[, ID := factor_in_order(ID)]
  }
  nodes@node_data <- setkey(node_data, ID)

  if (validObject(nodes))
    return(nodes)
}
