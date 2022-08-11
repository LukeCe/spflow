#' @include class_generics_and_maybes.R

#' @title Class spflow_pairs
#'
#' @description
#' An S4 class which holds information on origin-destination (OD) pairs.
#' Each OD pair is composed of two nodes.
#' All origins belong to the same set of nodes and all destination too
#' (see [spflow_nodes-class()]).
#' When the origins and the destinations belong to the same set of nodes the
#' corresponding ids should be equal
#'
#'
#' @slot id_spflow_pairs
#'   A character identifying the set of origin-destination pairs
#' @slot id_orig_nodes
#'   A character that serves as identifier for the origin nodes
#' @slot id_dest_nodes
#'   A character that serves as identifier for the destination network
#' @slot pair_data
#'   A data.frame containing information on origin-destination pairs
#'
#' @param object A spflow_pairs-class
#' @param value An object to replace the existing id/data
#' @family spflow network classes
#' @importClassesFrom Matrix Matrix
#' @name spflow_pairs-class
#' @export
setClass("spflow_pairs", slots = c(
  id_spflow_pairs = "character",
  id_orig_nodes   = "character",
  id_dest_nodes   = "character",
  pair_data       = "maybe_data.frame"))

# ---- Methods ----------------------------------------------------------------

# ---- ... dat ----------------------------------------------------------------
#' @rdname spflow_pairs-class
#' @export
#' @examples
#' ## access the data describing the node pairs
#'
#' net_pair_ge_ge <- pull_member(multi_net_usa_ge,"ge_ge")
#' dat(net_pair_ge_ge)
#'
setMethod(
  f = "dat",
  signature = "spflow_pairs", function(object) {
    return(object@pair_data)
    })

# ---- ... dat <- -------------------------------------------------------------
#' @rdname spflow_pairs-class
setReplaceMethod(
  f = "dat",
  signature = "spflow_pairs", function(object, value) {

    object@pair_data <- value
    validObject(object)
    return(object)
  })

# ---- ... id -----------------------------------------------------------------
#' @rdname spflow_pairs-class
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
  signature = "spflow_pairs",
  function(object) {
    return(c(
      "pair" = object@id_spflow_pairs,
      "orig" = object@id_orig_nodes,
      "dest" = object@id_dest_nodes
    ))
  })


# ---- ... id <- --------------------------------------------------------------
#' @rdname spflow_pairs-class
#' @param which
#'   A character indicating which of the ids to change, should be one of
#'   `c("origin", "destination", "pair")`.
#'   The first characters may be used as shortcuts.
#' @export
setReplaceMethod(
  f = "id",
  signature = "spflow_pairs",
  function(object, value, which = "pair") {

    assert_is_single_x(value, "character")
    is_which <- function(str) grepl(which, str, fixed = TRUE)

    if (is_which("pair"))
      object@id_spflow_pairs <- value

    if (is_which("orig"))
      object@id_orig_nodes <- value

    if (is_which("dest"))
      object@id_dest_nodes <- value

    return(object)
  })


# ---- ... npairs -------------------------------------------------------------
#' @rdname spflow_pairs-class
#' @export
#' @examples
#' ## access the number of node pairs in a network pair
#'
#' net_pair_ge_ge <- pull_member(multi_net_usa_ge,"ge_ge")
#' npairs(net_pair_ge_ge)
#'
setMethod(
  f = "npairs",
  signature = "spflow_pairs",
  function(object) {
    return(nrow(dat(object)))
  })


# ---- ... nnodes -------------------------------------------------------------
#' @rdname spflow_pairs-class
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
  signature = "spflow_pairs",
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
  signature = "spflow_pairs",
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


# ---- ... update_dat ---------------------------------------------------------
#' @rdname spflow_pairs-class
#' @param new_dat A data.frame
#' @export
#'
setMethod(
  f = "update_dat",
  signature = "spflow_pairs",
  function(object, new_dat) {

    # browser()
    assert(is_column_subset(dat(object), new_dat),
           'All columns in new_dat must exist and have the same
           type as in the pair_data of "%s"!', id(object)["pair"])

    new_cols <- colnames(new_dat)
    keys <- get_keycols(dat(object), no_coords = TRUE)
    assert(all(keys %in% new_cols),
           'The new_dat for spflow_pairs with id "%s"
           must have the column %s to identify the pairs!',
           id(object)["pair"], deparse(keys))

    okeys <- keys[2]
    dkeys <- keys[1]
    new_dat[[okeys]] <- factor(new_dat[[okeys]], levels(dat(object)[[okeys]]))
    new_dat[[dkeys]] <- factor(new_dat[[dkeys]], levels(dat(object)[[dkeys]]))
    all_nodes_known <- !any(is.na(new_dat[[okeys]]),is.na(new_dat[[dkeys]]))
    assert(all_nodes_known,
           'Some origins or destinations in new_dat do not correpond to
           observations in spflow_pairs with id "%s"!',
           id(object)["pair"])

    new_pair_indexes <- derive_pair_index_do(new_dat,keys)
    old_pair_indexes <- derive_pair_index_do(dat(object))
    new_dat_index <- match(new_pair_indexes,old_pair_indexes)
    assert(none(is.na(new_dat_index)) && has_distinct_elements(new_dat_index),
           'Some od pairs in new_dat are duplicated or do not correspond to
           observations in spflow_pairs with id "%s"!', id(object)["pair"])

    new_dat[[okeys]] <- NULL
    new_dat[[dkeys]] <- NULL
    dat(object)[new_dat_index, colnames(new_dat)] <- new_dat
    return(object)
  })
# ---- ... validity -----------------------------------------------------------
setValidity("spflow_pairs", function(object) {

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
#' @param id_orig_nodes
#'   A character that serves as identifier for the origin network
#' @param id_dest_nodes
#'   A character that serves as identifier for the destination network
#' @param id_spflow_pairs
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
#' @return An S4 class of type [spflow_pairs-class()]
#' @family Constructors for spflow network classes
#' @export
#' @examples
#' pair_frame <- data.frame(
#'   ORIG_ID_STATE = rep(germany_grid$ID_STATE, times = 16),
#'   DEST_ID_STATE = rep(germany_grid$ID_STATE, each = 16))
#' spflow_pairs("ge","ge","ge_ge",pair_frame,"ORIG_ID_STATE","DEST_ID_STATE")
spflow_pairs <- function(
  id_orig_nodes,
  id_dest_nodes,
  id_spflow_pairs = paste0(id_orig_nodes,"_",id_dest_nodes),
  pair_data = NULL,
  orig_key_column,
  dest_key_column
) {

  network_pair <- new(
    "spflow_pairs",
    id_orig_nodes      = id_orig_nodes,
    id_dest_nodes      = id_dest_nodes,
    id_spflow_pairs  = id_spflow_pairs,
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

