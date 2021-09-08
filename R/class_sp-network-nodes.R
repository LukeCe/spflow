#' @title sp_network_nodes Class
#'
#' @description
#' An S4 class that contains all information on a single network.
#' In this representation a network is composed of nodes which must be
#' identified uniquely by and ID.
#' Each node is described by variables stored in a data.frame.
#' The node neighborhood matrix describes strength of links between the nodes
#' of the network.
#' The class is constructed by the [sp_network_nodes()] function.
#'
#' @slot network_id
#'   A character that serves as an identifier for the network
#' @slot nnodes
#'   A numeric that indicates the number of nodes in the network
#' @slot node_data
#'   A data.frame that contains all information describing the nodes
#' @slot node_neighborhood
#'   A matrix that describes the neighborhood relations of the nodes
#'
#' @param object A sp_network_nodes-class
#' @param value An object to replace the existing id/data/neighborhood
#' @importClassesFrom Matrix Matrix
#' @family spflow network classes
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
#' new_dat <- dat(germany_net)
#'
setMethod(
  f = "dat",
  signature = "sp_network_nodes",
  function(object) { # ---- dat -----------------------------------------------
    return(object@node_data)
    })

#' @rdname sp_network_nodes-class
setReplaceMethod(
  f = "dat",
  signature = "sp_network_nodes", function(object, value) { # ---- dat <- -----

    object@node_data <- value
    object@nnodes <- nrow(value) %||% object@nnodes
    validObject(object)
    return(object)
    })

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
    assert(valid_network_id(value), "The network id is invalid!")
    object@network_id <- value
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
setReplaceMethod(
  f = "neighborhood",
  signature = "sp_network_nodes",
  function(object,value) { # ---- neighborhood <- -----------------------------

    object@node_neighborhood <- value %|!|% try_coercion(value,"Matrix")
    validObject(object)
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
    if (!valid_network_id(id(object))) {
      error_msg <- "The network id must contain only alphanumeric characters!"
      return(error_msg)
    }

    # check dimensions of nb matrix
    dim_nb <- dim(neighborhood(object))
    if (!has_equal_elements(dim_nb)) {
      error_msg <- "The neighborhood matrix must be a square matrix!"
      return(error_msg)
    }

    # check content of nb matrix
    if (!is.null(neighborhood(object))
        && any(diag(neighborhood(object)) != 0)) {
      error_msg <-
        "The neighborhood matrix must have zeros on the main diagonal!"
      return(error_msg)
    }

    # check dimensions of data and matrix
    nr_dat <- nrow(dat(object))
    nnodes <- nnodes(object)
    if (!has_equal_elements(c(nr_dat,dim_nb,nnodes))) {
      error_msg <-
        "The row number of the node_data does not match the dimensions " %p%
        "of the neighborhood matrix!"
      return(error_msg)
    }

    # check details of the data
    if (is.null(dat(object)))
      return(TRUE)

    node_id_col <- attr_key_nodes(dat(object))
    if (is.null(node_id_col)) {
      error_msg <- "The data musst have a key column!"
      return(error_msg)
    }

    node_ids <- dat(object)[[node_id_col]]
    duplicated_ids <- length(unique(node_ids)) != nr_dat
    wrong_id_type <- !is.factor(node_ids)
    if (duplicated_ids | wrong_id_type ) {
      error_msg <-
        "The nodes are not correctly identifyed.\n Please ensure " %p%
        "that all entries in column " %p% node_id_col %p% " are unique!"
      return(error_msg)
    }

    # object is valid
    return(TRUE)
  })

# ---- Constructors -----------------------------------------------------------

#' Create a [sp_network_nodes-class()]
#'
#' @param network_id
#'   A character that serves as an identifier for the network
#' @param node_data
#'   A data.frame that contains all information describing the nodes
#' @param node_neighborhood
#'   A matrix that describes the neighborhood of the nodes
#' @param node_key_column
#'   A character indicating the column containing the identifiers for the nodes
#'
#' @family Constructors for spflow network classes
#' @importClassesFrom Matrix Matrix
#' @return An S4 class of type [sp_network_nodes-class()]
#' @export
#' @examples
#' sp_network_nodes("germany",
#'                  spdep::nb2mat(spdep::poly2nb(germany_grid)),
#'                  as.data.frame(germany_grid),
#'                  "ID_STATE")
sp_network_nodes <- function(
  network_id,
  node_neighborhood = NULL,
  node_data = NULL,
  node_key_column
) {


  # checks for validity of dimensions are done before the return
  node_neighborhood <- node_neighborhood %|!|%
    try_coercion(node_neighborhood,"Matrix")

  dim_neighborhood <- dim(node_neighborhood)
  dim_node_data <- dim(node_data)
  nnodes <- c(dim_node_data[1],dim_neighborhood)
  nnodes <- unique(nnodes)[[1]]

  nodes <- new(
    "sp_network_nodes",
    network_id        = network_id,
    node_neighborhood = node_neighborhood,
    nnodes            = nnodes,
    node_data         = NULL)

  if (is.null(node_data))
    return(nodes)

  # Add the key column to the node_data
  assert_inherits(node_data, "data.frame")

  if (missing(node_key_column)) node_key_column <- attr_key_nodes(node_data)
  assert_is_single_x(node_key_column,"character")
  assert(node_key_column %in% names(node_data),
         "The node_key_column is not found in the node_data!")

  attr_key_nodes(node_data) <- node_key_column
  node_data[[node_key_column]] <- factor_in_order(node_data[[node_key_column]])
  nodes@node_data <- node_data

  validObject(nodes)
  return(nodes)
}

# ---- Helpers ----------------------------------------------------------------

#' @keywords internal
attr_key_nodes <- function(df) {
  attr(df, "node_key_column")
}

#' @keywords internal
`attr_key_nodes<-` <- function(df, value) {
  attr(df, "node_key_column") <- value
  df
}

#' @keywords internal
valid_network_id <- function(key) {
  is_single_character(key) && grepl("^[[:alnum:]]+$",key)
}
