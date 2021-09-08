#' @title Class sp_network_pair
#'
#' @description
#' An S4 class which holds information on origin-destination (OD) pairs.
#' Each OD pair is composed of two nodes (see [sp_network_nodes-class()]).
#' All origins belong to the same (origin-) network and all destination belong
#' to the same (destination-) network. It is possible to choose the same
#' network for origins and destinations, which enables to represent OD pairs
#' within the same network.
#'
#'
#' @slot orig_net_id
#'   A character that serves as identifier for the origin network
#' @slot orig_nnodes
#'   A numeric that represents the number of nodes in the origin network
#' @slot dest_net_id
#'   A character that serves as identifier for the destination network
#' @slot dest_nnodes
#'   A numeric that represents the number of nodes in the destination network
#' @slot network_pair_id
#'   A character identifying the pair of networks
#' @slot pair_data
#'   A data.frame containing information on origin-destination pairs
#' @slot npairs
#'   A numeric indicating the number of origin-destination pairs
#'
#' @param object A sp_network_pair-class
#' @param value An object to replace the existing id/data
#' @family spflow network classes
#' @importClassesFrom Matrix Matrix
#' @name sp_network_pair-class
#' @export
setClass("sp_network_pair",
         slots = c(orig_net_id     = "character",
                   orig_nnodes     = "maybe_numeric",
                   dest_net_id     = "character",
                   dest_nnodes     = "maybe_numeric",
                   network_pair_id = "character",
                   pair_data       = "maybe_data.frame",
                   npairs          = "maybe_numeric"))

# ---- Methods ----------------------------------------------------------------


#' @rdname sp_network_pair-class
#' @export
#' @examples
#' ## access the data describing the node pairs
#'
#' net_pair_ge_ge <- pull_member(multi_net_usa_ge,"ge_ge")
#' dat(net_pair_ge_ge)
#'
setMethod(
  f = "dat",
  signature = "sp_network_pair", function(object) {  # ---- dat ---------------
    return(object@pair_data)
    })

#' @rdname sp_network_pair-class
setReplaceMethod(
  f = "dat",
  signature = "sp_network_pair", function(object, value) {  # ---- dat <- -----

    object@pair_data <- value
    object@npairs <- nrow(value)
    validObject(object)
    return(object)
  })

#' @rdname sp_network_pair-class
#' @export
#' @examples
#' ## access the id of a network pair
#'
#' net_pair_ge_ge <- pull_member(multi_net_usa_ge,"ge_ge")
#' id(net_pair_ge_ge)
#' id(net_pair_ge_ge) <- "Germany_Germany"
#'
setMethod(
  f = "id",
  signature = "sp_network_pair",
  function(object) { # ---- id -----------------------------------
    return(c(
      "pair" = object@network_pair_id,
      "orig" = object@orig_net_id,
      "dest" = object@dest_net_id
    ))
  })


#' @rdname sp_network_pair-class
#' @export
setReplaceMethod(
  f = "id",
  signature = "sp_network_pair",
  function(object,value) {  # ---- id <- --------------------------------------

    split_ids   <- unlist(strsplit(value,"_"))
    new_id_orig <- split_ids[1]
    new_id_dest <- split_ids[2]

    object@orig_net_id <- new_id_orig
    object@dest_net_id <- new_id_dest
    object@network_pair_id <- value
    validObject(object)
    return(object)
  })


#' @rdname sp_network_pair-class
#' @export
#' @examples
#' ## access the number of node pairs in a network pair
#'
#' net_pair_ge_ge <- pull_member(multi_net_usa_ge,"ge_ge")
#' npairs(net_pair_ge_ge)
#'
setMethod(
  f = "npairs",
  signature = "sp_network_pair",
  function(object) { # ---- npairs --------------------------------------------
    return(object@npairs)
  })

#' @rdname sp_network_pair-class
#' @export
#' @examples
#' ## access the number of origin and destination nodes in a network pair

#' net_pair_ge_ge <- pull_member(multi_net_usa_ge,"ge_ge")
#' nnodes(net_pair_ge_ge)
#' nnodes(net_pair_ge_ge)["orig"]
#' nnodes(net_pair_ge_ge)["dest"]
#' prod(nnodes(net_pair_ge_ge) == npairs(net_pair_ge_ge))
#'
setMethod(
  f = "nnodes",
  signature = "sp_network_pair",
  function(object) { # ---- nnodes --------------------------------------------
    return(c(
      "orig" = object@orig_nnodes,
      "dest" = object@dest_nnodes
    ))
  })


#' @keywords internal
setMethod(
  f = "show",
  signature = "sp_network_pair",
  function(object){ # ---- show -----------------------------------------------

    cat("Spatial network pair with id:",id(object)["pair"])
    cat("\n")
    cat(print_line(50))

    od_explain <- "\n%s network id: %s (with %s nodes)"

    cat(sprintf(od_explain,
                "Origin", id(object)["orig"],
                nnodes(object)["orig"] %||% "[?]"))
    cat(sprintf(od_explain,
                "Destination", id(object)["dest"],
                nnodes(object)["dest"]  %||% "[?]"))

    has_all_counts <- length(c(npairs(object),nnodes(object))) == 3
    if (has_all_counts) {
      cat("\nNumber of pairs:", npairs(object))
      pair_explain <- "\nCompleteness of pairs: %s (%i/%i)"
      cat(sprintf(pair_explain,
                  format_percent(npairs(object) / prod(nnodes(object))),
                  npairs(object),
                  prod(nnodes(object))
                  ))
    }

    has_data <- !is.null(dat(object))
    if (has_data) {
      cat("\n\nData on node-pairs:\n")
      print(dat(object))
    }
    cat("\n")
    invisible(object)
  })

setValidity("sp_network_pair", function(object) { # ---- validity -------------

  # check ids
  ids <- id(object)
  if (!valid_network_pair_id(ids["pair"])) {
    error_msg <-
      "The id of the pair object is invalid.\n Please ensure that the id " %p%
      "is based on two alphanumeric strings sperated by an underscore, " %p%
      "as for example 'alnum1_alnum2'!"
    return(error_msg)
  }

  # check plausibility and identifiability of the data
  if (is.null(dat(object)))
    return(TRUE)

  data_keys <- attr_key_od(dat(object))
  keys_exist <- all(data_keys %in% names(dat(object)))

  if (is(dat(object),"data.table")) {
    data_keys <- unique(dat(object)[,data_keys, with = FALSE])
  } else {
    data_keys <- unique(dat(object)[,data_keys, drop = FALSE])

  }

  unique_identification <- nrow(data_keys) == nrow(dat(object))

  if (!all(keys_exist, unique_identification)) {
    error_msg <-
      "Based on the origin and destination key columns the observations " %p%
      "are not unequely identifyed!"
    return(error_msg)
  }

  # The object is valid
  return(TRUE)
})
# ---- Constructors -----------------------------------------------------------

#' Create an S4 object that contains information on origin-destination pairs
#'
#' @param orig_net_id
#'   A character that serves as identifier for the origin network
#' @param dest_net_id
#'   A character that serves as identifier for the destination network
#' @param pair_data
#'   A data.frame containing information on the origin-destination pairs
#' @param orig_key_column
#'   A character indicating the name of the column containing the identifiers
#'   of the origins
#' @param dest_key_column
#'   A character indicating the name of the column containing the identifiers
#'   of the destinations
#'
#' @return An S4 class of type [sp_network_pair-class()]
#' @family Constructors for spflow network classes
#' @export
#' @examples
#' pair_frame <- data.frame(
#'   ORIG_ID_STATE = rep(germany_grid$ID_STATE, times = 16),
#'   DEST_ID_STATE = rep(germany_grid$ID_STATE, each = 16))
#' sp_network_pair("ge","ge",pair_frame,"ORIG_ID_STATE","DEST_ID_STATE")
sp_network_pair <- function(
  orig_net_id,
  dest_net_id,
  pair_data = NULL,
  orig_key_column,
  dest_key_column
) {

  network_pair <- new(
    "sp_network_pair",
    orig_net_id      = orig_net_id,
    orig_nnodes      = NULL,
    dest_net_id      = dest_net_id,
    dest_nnodes      = NULL,
    network_pair_id  = orig_net_id %p% "_" %p% dest_net_id,
    pair_data        = NULL,
    npairs           = NULL)

  # early return with empty counts when no data was provided
  if (is.null(pair_data) && validObject(network_pair))
    return(network_pair)

  # when the data is provided there must be valid key columns...
  assert_inherits(pair_data, "data.frame")
  od_key_cols <- c(orig_key_column, dest_key_column)
  has_orig_key <- orig_key_column %in% colnames(pair_data)
  has_dest_key <-
  assert(all(od_key_cols %in% colnames(pair_data)),
         "The origin and destination key columns are not found in " %p%
         "the pair data!")

  # convert to factor
  attr_key_od(pair_data) <- od_key_cols
  pair_data[[od_key_cols[1]]] <- factor_in_order(pair_data[[od_key_cols[1]]])
  pair_data[[od_key_cols[2]]] <- factor_in_order(pair_data[[od_key_cols[2]]])
  pair_data <- pair_data[order(pair_data[[od_key_cols[1]]],
                               pair_data[[od_key_cols[2]]]), ]

  network_pair@pair_data   <- pair_data
  network_pair@orig_nnodes <- nlevels(pair_data[[od_key_cols[1]]])
  network_pair@dest_nnodes <- nlevels(pair_data[[od_key_cols[2]]])
  network_pair@npairs      <- nrow(pair_data)

  validObject(network_pair)
  return(network_pair)
}


# ---- Functions --------------------------------------------------------------
#' @importFrom Matrix sparseMatrix
#' @keywords internal
matrix_form_control <- function(sp_net_pair) {

  matrix_arguments <- list(
    "mat_complet" = npairs(sp_net_pair) / prod(nnodes(sp_net_pair)),
    "mat_within" = id(sp_net_pair)["orig"] == id(sp_net_pair)["dest"],
    "mat_npairs" = npairs(sp_net_pair),
    "mat_nrows" = nnodes(sp_net_pair)["orig"],
    "mat_ncols" = nnodes(sp_net_pair)["dest"],
    "mat_format" = NULL)

  if (matrix_arguments[["mat_complet"]] == 1) {
    matrix_arguments[["mat_format"]] <- function(vec) {
      matrix(vec,
             nrow = matrix_arguments[["mat_nrows"]],
             ncol = matrix_arguments[["mat_ncols"]])
    }
  }

  if (matrix_arguments[["mat_complet"]] < 1) {
    od_keys <- attr_key_od(dat(sp_net_pair))
    mat_i_rows <- as.integer(dat(sp_net_pair)[[od_keys[1]]])
    mat_j_cols <- as.integer(dat(sp_net_pair)[[od_keys[2]]])
    matrix_arguments[["mat_format"]] <- function(vec) {
      mat <- matrix(0,
                    nrow = matrix_arguments[["mat_nrows"]],
                    ncol = matrix_arguments[["mat_ncols"]])
      mat[cbind(mat_i_rows, mat_j_cols)] <- vec
      mat

    }
  }

  if (matrix_arguments[["mat_complet"]] < .5) {
    od_keys <- attr_key_od(dat(sp_net_pair))
    matrix_arguments[["mat_format"]] <- function(vec) {
      sparseMatrix(i= mat_i_rows, j=mat_j_cols,
                   x= vec,
                   dims = c(matrix_arguments[["mat_nrows"]],
                            matrix_arguments[["mat_ncols"]]))
    }
  }

  return(matrix_arguments)
}


# ---- Helpers ----------------------------------------------------------------
#' @keywords internal
split_pair_id <- function(pair_id){
  strsplit(pair_id,"_")[[1]]
}

#' @keywords internal
attr_key_orig <- function(df) {
  attr(df, "orig_key_column")
}

#' @keywords internal
`attr_key_orig<-` <- function(df, value) {
  attr(df, "orig_key_column") <- value
  df
}

#' @keywords internal
attr_key_dest <- function(df) {
  attr(df, "dest_key_column")
}

#' @keywords internal
`attr_key_dest<-` <- function(df, value) {
  attr(df, "dest_key_column") <- value
  df
}


#' @keywords internal
attr_key_od <- function(df) {
  c(attr_key_orig(df),
    attr_key_dest(df))
}

#' @keywords internal
`attr_key_od<-` <- function(df, value) {
  attr_key_orig(df) <- value[1]
  attr_key_dest(df) <- value[2]
  df
}

#' @keywords internal
valid_network_pair_id <- function(key) {
  split_strings <- unlist(strsplit(key,"_",fixed = TRUE))
  is_single_character(key) &&
    length(split_strings) == 2 &&
    valid_network_id(split_strings[1]) &&
    valid_network_id(split_strings[2])
}

