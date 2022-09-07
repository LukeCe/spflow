#' @include class_generics_and_maybes.R

#' @title spflow_network_nodes Class
#'
#' @description
#' An S4 class that contains all information on a single network.
#' In this representation a network is composed of nodes which must be
#' identified uniquely by and ID.
#' Each node is described by variables stored in a data.frame.
#' The node neighborhood matrix describes strength of links between the nodes
#' of the network.
#' The class is constructed by the [spflow_network_nodes()] function.
#'
#' @slot id_spflow_network_nodes
#'   A character that serves as an identifier for the set of nodes
#' @slot node_data
#'   A data.frame that contains all information describing the nodes
#' @slot node_neighborhood
#'   A matrix that describes the neighborhood relations of the nodes
#'
#' @param object A spflow_network_nodes-class
#' @param value An object to replace the existing id/data/neighborhood
#' @importClassesFrom Matrix Matrix
#' @name spflow_network_nodes-class
#' @export
setClass("spflow_network_nodes",
         slots = c(
           id_spflow_network_nodes   = "character",
           node_neighborhood = "maybe_Matrix",
           node_data         = "maybe_data.frame"))

# ---- Methods ----------------------------------------------------------------
# ---- ... dat ----------------------------------------------------------------
#' @rdname spflow_network_nodes-class
#' @export
#' @examples
#' ## access the data describing the nodes
#' new_dat <- dat(germany_net)
#'
setMethod(
  f = "dat",
  signature = "spflow_network_nodes",
  function(object) {
    return(object@node_data)
    })

# ---- ... dat <- -------------------------------------------------------------
#' @rdname spflow_network_nodes-class
setReplaceMethod(
  f = "dat",
  signature = "spflow_network_nodes", function(object, value) {

    object@node_data <- value
    validObject(object)
    return(object)
    })

# ---- ... id -----------------------------------------------------------------
#' @rdname spflow_network_nodes-class
#' @examples
#' # access the id of the network
#' germany_net2 <- germany_net
#' id(germany_net2)
#' id(germany_net2) <- "Germany"
#'
setMethod(
  f = "id",
  signature = "spflow_network_nodes",
  function(object) {
    return(object@id_spflow_network_nodes)
  })

# ---- ... id <- --------------------------------------------------------------
#' @rdname spflow_network_nodes-class
#' @export
setReplaceMethod(
  f = "id",
  signature = "spflow_network_nodes",
  function(object, value) {
    assert(valid_spflow_network_nodes_id(value), "The id is invalid!")
    object@id_spflow_network_nodes <- value
    return(object)
  })

# ---- ... neighborhood -------------------------------------------------------
#' @rdname spflow_network_nodes-class
#' @export
#' @examples
#' # access the neighborhood matrix of the nodes
#' neighborhood(germany_net)
#'
setMethod(
  f = "neighborhood",
  signature = "spflow_network_nodes",
  function(object) return(object@node_neighborhood))

# ---- ... neighborhood <- ----------------------------------------------------
#' @rdname spflow_network_nodes-class
setReplaceMethod(
  f = "neighborhood",
  signature = "spflow_network_nodes",
  function(object, value) {


    if (!is.null(value)) {
      value <- try_coercion(value,"Matrix")

      assert(has_equal_elements(c(dim(value), nnodes(object))),
             "Replacement neighborhood musst have %s rows and %s columns!",
             nnodes(object), nnodes(object))

      value <- normalize_neighborhood(value)
    }

    object@node_neighborhood <- value
    validObject(object)
    return(object)
  })

# ---- ... nnodes -------------------------------------------------------------
#' @rdname spflow_network_nodes-class
#' @export
#' @examples
#' # access the number of nodes inside the network
#' nnodes(germany_net)
#'
setMethod(
  f = "nnodes",
  signature = "spflow_network_nodes",
  function(object) {
    dims <- c(nrow(object@node_data), nrow(object@node_neighborhood))
    return(dims %|!|% max)
  })


# ---- ... show ---------------------------------------------------------------
#' @keywords internal
setMethod(
  f = "show",
  signature = "spflow_network_nodes",
  function(object){

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
      pprint_df(dat(object))
    }
    cat("\n")
    invisible(object)
  })



# ---- ... update_dat ---------------------------------------------------------
#' @rdname spflow_network_nodes-class
#' @param new_dat A data.frame
#' @export
#'
setMethod(
  f = "update_dat",
  signature = "spflow_network_nodes",
  function(object, new_dat) {

    assert(is_column_subset(dat(object), new_dat),
           'All columns in new_dat must exist and have the same
           type as in the node_data of "%s"!', id(object))

    new_cols <- colnames(new_dat)
    keys <- get_keycols(dat(object), no_coords = TRUE)
    assert(all(keys %in% new_cols),
           'The new_dat for spflow_network_nodes with id "%s"
           must have the column %s to identify the nodes!',
           id(object), deparse(keys))

    new_dat[[keys]] <- factor(new_dat[[keys]], levels(dat(object)[[keys]]))
    assert(!any(is.na(new_dat)) && has_distinct_elements(new_dat[[keys]]),
           'Some keys in new_dat are duplicated or do not correpond to
           observations in spflow_network_nodes with id "%s"!',
           id(object))

    new_dat_index <- as.numeric(new_dat[[keys]])
    new_dat[[keys]] <- NULL
    new_cols <- setdiff(colnames(new_dat), keys)
    dat(object)[new_dat_index, new_cols] <- new_dat
    return(object)
  })

# ---- ... validity -----------------------------------------------------------
setValidity(
  Class = "spflow_network_nodes",
  function(object) {

    check <- "The network id must contain only alphanumeric characters!"
    if (!valid_spflow_network_nodes_id(id(object)))
      return(check)

    # verify details of the neighborhood
    dim_nb <- dim(neighborhood(object))
    if (!is.null(dim_nb)) {

      check <- "The neighborhood matrix must be a square!"
      if (!has_equal_elements(dim_nb))
        return(check)

      check <- "The neighborhood matrix must have zeros on the main diagonal!"
      if (any(diag(neighborhood(object)) != 0))
        return(check)

      check <- "The neighborhood matrix must have non-negative entries!"
      if (any(neighborhood(object) < 0))
        return(check)

      # TODO Should normalization of the nb matrix be required in spflow_network_nodes?
      # check <- "The neighborhood matrix should be normalized!"
      # spectral_radius <- attr_spectral_character(neighborhood(object))
      # spectral_radius <- abs(spectral_radius[["LM"]])
      # tol <- sqrt(.Machine$double.eps)
      # if ((spectral_radius - tol) > 1)
      #   return(check)
      # if ((spectral_radius + tol) < 1) {
      #   byrow_norm <- all(abs(rowSums(neighborhood(object)) - .5) == .5)
      #   if (!byrow_norm)
      #     return(check)
      # }
    }

    # verify details of the node data
    nnodes <- nrow(dat(object))
    if (!is.null(nnodes)) {

      check <- "The dimension of the neighborhood musst match the number of nodes!"
      if (!has_equal_elements(c(nnodes, dim_nb)))
        return(check)

      check <- "The data musst have a key column!"
      node_id_col <- attr_key_nodes(dat(object))
      if (is.null(node_id_col))
        return(check)

      check <- "The key-column musst be a factor!"
      node_ids <- dat(object)[[node_id_col]]
      if (!is.factor(node_ids))
        return(check)

      check <- "All entries in the key-column musst be unique!"
      if (!has_distinct_elements(node_ids))
        return(check)

      check <- "The node data musst be ordered according to the key-column!"
      if (is.unsorted(node_ids))
        return(check)

    }

    # object is valid
    return(TRUE)
  })

# ---- Constructors -----------------------------------------------------------

#' Create a [spflow_network_nodes-class()]
#'
#' @param id_spflow_network_nodes
#'   A character that serves as an identifier for the set of nodes
#' @param node_data
#'   A data.frame that contains all information describing the nodes
#' @param node_neighborhood
#'   A matrix that describes the neighborhood of the nodes
#' @param node_key_column
#'   A character indicating the column containing the identifiers for the nodes
#' @param derive_coordinates
#'   A logical indicating whether there should be an attempt to infer the
#'   coordinates from the node_data.
#' @param prefer_lonlat
#'   A logical indicating whether the coordinates should be transformed to
#'   longitude and latitude.
#' @param node_coord_columns
#'   A character indicating the columns that represent the coordinates of the
#'   nodes. For example `c("LON", "LAT")`.
#' @param normalize_byrow
#'   A logical, if `TRUE` the neighborhood will be row-normalized, otherwise
#'   it is scaled to have a spectral radius of one.
#'
#' @importClassesFrom Matrix Matrix
#' @return An S4 class of type [spflow_network_nodes-class()]
#' @export
#' @examples
#' spflow_network_nodes(
#'   "germany",
#'   spdep::nb2mat(spdep::poly2nb(germany_grid)),
#'   as.data.frame(germany_grid),
#'   "ID_STATE")
spflow_network_nodes <- function(
  id_spflow_network_nodes,
  node_neighborhood = NULL,
  node_data = NULL,
  node_key_column,
  node_coord_columns,
  derive_coordinates = missing(node_coord_columns),
  prefer_lonlat = TRUE,
  normalize_byrow = FALSE) {


  # checks for validity of dimensions are done before the return
  if (!is.null(node_neighborhood)) {
    node_neighborhood <- try_coercion(node_neighborhood,"Matrix")
    node_neighborhood <- normalize_neighborhood(node_neighborhood, normalize_byrow)
  }

  nodes <- new("spflow_network_nodes",
    id_spflow_network_nodes   = id_spflow_network_nodes,
    node_neighborhood = node_neighborhood,
    node_data         = NULL)

  if (is.null(node_data))
    return(nodes)

  node_data <- simplfy2df(
    node_data,
    derive_coord_cols = derive_coordinates,
    prefer_lonlat = prefer_lonlat)

  # identfyers for nodes and coordinates
  if (missing(node_key_column))
    node_key_column <- attr_key_nodes(node_data)
  node_data[[node_key_column]] <- factor_in_order(node_data[[node_key_column]])
  attr_key_nodes(node_data) <- node_key_column

  if (missing(node_coord_columns))
    node_coord_columns <- attr_coord_col(node_data)
  attr_coord_col(node_data) <- node_coord_columns

  if (inherits(node_data, "data.table") && requireNamespace("data.table", quietly = TRUE))
    node_data <- data.table::as.data.table(node_data)

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

  assert(sum(value == names(df)) == 1, "
         The node_key_column musst unquily identfy one
         column name of in the node_data!")
  attr(df, "node_key_column") <- value
  df
}

#' @keywords internal
attr_coord_col <- function(df, value) {
  attr(df, "coord_columns")
}

#' @keywords internal
`attr_coord_col<-` <- function(df, value) {
  assert(sum(value %in% names(df)) == length(value), "
         The coord_columns musst unquily identfy the corresponding
         column names in the node_data!")
  attr(df, "coord_columns") <- value
  df
}

#' @keywords internal
valid_spflow_network_nodes_id <- function(key) {
  is_single_character(key) && grepl("^[[:alnum:]]+$",key)
}

#' @description  Convert spatial data to a simple data.frame
#' @noRd
#' @keywords internal
simplfy2df <- function(df, derive_coord_cols = TRUE, prefer_lonlat = TRUE) {


  dt_used <- inherits(df, "data.table")
  sf_convertible <- inherits(df, "Spatial") || inherits(df, "sfc") || any(sapply(df, inherits, "sfc"))

  if (sf_convertible && requireNamespace("sf", quietly = TRUE))
    df <- sf::st_as_sf(df)
  if (inherits(df, "Spatial"))
    df <- as.data.frame(df@data)

  if (inherits(df, "sf")) {

    if (derive_coord_cols && requireNamespace("sf", quietly = TRUE)) {
      coords <- sf::st_geometry(df)
      coords <- suppressWarnings(sf::st_point_on_surface(coords))
      if (!is.na(sf::st_crs(coords)) & prefer_lonlat)
        coords <- sf::st_transform(coords, "WGS84")

      coords <- prefix_columns(data.frame(sf::st_coordinates(coords)), "COORD_")
      df <- cbind(sf::st_drop_geometry(df), coords)
      names(df) <- make.names(names = names(df), unique = TRUE)
      attr_coord_col(df) <- rev(names(df))[seq(ncol(coords), 1)]
    } else {
      df <- as.data.frame(df)
      df <- Filter("is.atomic", df)
    }
  }

  if (dt_used && requireNamespace("data.table", quietly = TRUE))
    return(data.table::as.data.table(df))
  if (inherits(df, "data.frame"))
    return(df)
  stop("Data cannot be converted to a data.frame!")
}
