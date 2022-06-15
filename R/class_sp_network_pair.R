#' @include class_generics_and_maybes.R

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
#' @slot dest_net_id
#'   A character that serves as identifier for the destination network
#' @slot network_pair_id
#'   A character identifying the pair of networks
#' @slot pair_data
#'   A data.frame containing information on origin-destination pairs
#'
#' @param object A sp_network_pair-class
#' @param value An object to replace the existing id/data
#' @family spflow network classes
#' @importClassesFrom Matrix Matrix
#' @name sp_network_pair-class
#' @export
setClass("sp_network_pair",
         slots = c(orig_net_id     = "character",
                   dest_net_id     = "character",
                   network_pair_id = "character",
                   pair_data       = "maybe_data.frame"))

# ---- Methods ----------------------------------------------------------------

# ---- ... dat ----------------------------------------------------------------
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
  signature = "sp_network_pair", function(object) {
    return(object@pair_data)
    })

# ---- ... dat <- -------------------------------------------------------------
#' @rdname sp_network_pair-class
setReplaceMethod(
  f = "dat",
  signature = "sp_network_pair", function(object, value) {

    object@pair_data <- value
    validObject(object)
    return(object)
  })

# ---- ... id -----------------------------------------------------------------
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
  function(object) {
    return(c(
      "pair" = object@network_pair_id,
      "orig" = object@orig_net_id,
      "dest" = object@dest_net_id
    ))
  })


# ---- ... id <- --------------------------------------------------------------
#' @rdname sp_network_pair-class
#' @param which
#'   A character indicating which of the ids to change, should be one of
#'   `c("origin", "destination", "pair")`.
#'   The first characters may be used as shortcuts.
#' @export
setReplaceMethod(
  f = "id",
  signature = "sp_network_pair",
  function(object, value, which = "pair") {

    assert_is_single_x(value, "character")
    is_which <- function(str) grepl(which, str, fixed = TRUE)

    if (is_which("pair"))
      object@network_pair_id <- value

    if (is_which("orig"))
      object@orig_net_id <- value

    if (is_which("dest"))
      object@dest_net_id <- value

    return(object)
  })


# ---- ... npairs -------------------------------------------------------------
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
  function(object) {
    return(nrow(dat(object)))
  })


# ---- ... nnodes -------------------------------------------------------------
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
  function(object) {

    if (is.null(dat(object)))
      return(NULL)

    od_key_cols <- attr_key_od(dat(object))
    od_nnodes <- unlist(lapply(
      od_key_cols,
      function(.key) nlevels(dat(object)[[.key]])))
    return(od_nnodes)
  })


# ---- ... show ---------------------------------------------------------------
#' @keywords internal
setMethod(
  f = "show",
  signature = "sp_network_pair",
  function(object){

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

# ---- ... validity -----------------------------------------------------------
setValidity("sp_network_pair", function(object) {

  # check ids
  ids <- id(object)
  if (any(length(ids) != 3, !is.character(ids))) {
    error_msg <- sprintfwrap("
      The ids for the network pair object are invalid invalid!
      Please ensure that the origin, destination and network_pair ids are
      characters of length one.")
    return(error_msg)
  }

  # check plausibility and identifiability of the data
  if (is.null(dat(object)))
    return(TRUE)

  data_keys <- attr_key_od(dat(object))
  keys_exist <- all(data_keys %in% names(dat(object)))

  if (is(dat(object),"data.table")) {
    data_keys <- unique(dat(object)[,data_keys, drop = FALSE])
  } else {
    data_keys <- unique(dat(object)[,data_keys, drop = FALSE])
  }

  unique_identification <- nrow(data_keys) == nrow(dat(object))
  if (!all(keys_exist, unique_identification)) {
    error_msg <- "
    Based on the origin and destination key columns the observations
    are not unequely identifyed!"
    return(sprintfwrap(error_msg))
  }

  if (is.unsorted(get_pair_index(dat(object)))) {
    error_msg <- "
    The order of origin-destination pairs is invalid!"
    return(sprintfwrap(error_msg))
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
#' @param network_pair_id
#'   A character that as identifier for network_pair
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
  network_pair_id = paste0(orig_net_id,"_",dest_net_id),
  pair_data = NULL,
  orig_key_column,
  dest_key_column
) {

  network_pair <- new(
    "sp_network_pair",
    orig_net_id      = orig_net_id,
    dest_net_id      = dest_net_id,
    network_pair_id  = network_pair_id,
    pair_data        = NULL)

  # early return with empty counts when no data was provided
  if (is.null(pair_data) && validObject(network_pair))
    return(network_pair)

  # when the data is provided there must be valid key columns...
  assert_inherits(pair_data, "data.frame")
  do_key_cols <- c(dest_key_column,orig_key_column)
  assert(all(do_key_cols %in% colnames(pair_data)), "
         The origin and destination key columns are
         not found in the pair data!")

  # convert to factor
  do_keys <- lapply(pair_data[do_key_cols], "factor_in_order")
  pair_data[do_key_cols] <- do_keys

  order_names <- c(do_key_cols, setdiff(names(pair_data), do_key_cols))
  pair_data <- pair_data[order(do_keys[[2]],do_keys[[1]]), order_names]
  attr_key_do(pair_data) <- do_key_cols
  if (inherits(pair_data, "data.table") && requireNamespace("data.table", quietly = TRUE))
    pair_data <- data.table::as.data.table(pair_data)

  network_pair@pair_data   <- pair_data
  validObject(network_pair)
  return(network_pair)
}


# ---- Functions --------------------------------------------------------------
#' @importFrom Matrix sparseMatrix
#' @keywords internal
matrix_form_control <- function(sp_net_pair) {

  matrix_arguments <- list(
    "mat_complet" = npairs(sp_net_pair) / prod(nnodes(sp_net_pair)),
    "mat_within" = has_equal_elements(id(sp_net_pair)[c("orig","dest")]),
    "mat_npairs" = npairs(sp_net_pair),
    "mat_nrows" = nnodes(sp_net_pair)["dest"],
    "mat_ncols" = nnodes(sp_net_pair)["orig"],
    "mat_format" = function(vec) {

      od_keys <- attr_key_od(dat(sp_net_pair))
      dest_index <- as.integer(dat(sp_net_pair)[[od_keys[2]]])
      orig_index <- as.integer(dat(sp_net_pair)[[od_keys[1]]])
      num_dest <- nnodes(sp_net_pair)["dest"]
      num_orig <- nnodes(sp_net_pair)["orig"]

      matrix_format_d_o(
        values = vec,
        dest_index = dest_index,
        orig_index = orig_index,
        num_dest = num_dest,
        num_orig = num_orig,
        assume_ordered = TRUE)

    })
  return(matrix_arguments)
}


# ---- Helpers ----------------------------------------------------------------
#' @keywords internal
attr_key_orig <- function(df) {
  attr(df, "orig_key_column")
}

#' @keywords internal
`attr_key_orig<-` <- function(df, value) {
  assert(sum(names(df) == value) == 1,
         "The key column %s does not exist!", value)
  attr(df, "orig_key_column") <- value
  df
}

#' @keywords internal
attr_key_dest <- function(df) {
  attr(df, "dest_key_column")
}

#' @keywords internal
`attr_key_dest<-` <- function(df, value) {
  assert(sum(names(df) == value) == 1,
         "The key column %s does not exist!", value)
  attr(df, "dest_key_column") <- value
  df
}

#' @keywords internal
attr_key_od <- function(df) {
  c("orig" = attr_key_orig(df),
    "dest" = attr_key_dest(df))
}

#' @keywords internal
`attr_key_od<-` <- function(df, value) {
  attr_key_orig(df) <- value[1]
  attr_key_dest(df) <- value[2]
  df
}

#' @keywords internal
attr_key_do <- function(df) {
  c("dest" = attr_key_dest(df),
    "orig" = attr_key_orig(df))
}

#' @keywords internal
`attr_key_do<-` <- function(df, value) {
  attr_key_dest(df) <- value[1]
  attr_key_orig(df) <- value[2]
  df
}

#' @keywords internal
get_do_keys <- function(df, do_keys = attr_key_do(df)) {
  df <- df[,do_keys, drop = FALSE]
  row.names(df) <- as.integer(df[[1]]) + nlevels(df[[1]]) * (as.integer(df[[2]]) - 1)
  return(df)
}

#' @keywords internal
get_do_indexes <- function(df, do_keys = attr_key_do(df)) {
  Reduce("cbind", lapply(df[do_keys], "as.integer"), init = NULL)
}

#' @keywords internal
get_pair_index <- function(
  df,
  do_keys = attr_key_do(df),
  n_d = nlevels(df[[do_keys[1]]])) {

  do_ind <- get_do_indexes(df, do_keys)
  do_ind[,1] + n_d * (do_ind[,2] - 1)
}

