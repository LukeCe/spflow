#' @include class_sp-network-nodes.R class_sp-network-pair.R

setClassUnion("sp_network_meta" ,c("sp_network_nodes", "sp_network_pair"))

# ---- methods ----------------------------------------------------------------

#' @param col_subset A character vector indicating columns that should be kept
#'   for the template
#' @rdname dat_template
setMethod(
  f = "dat_template",
  signature = c("sp_network_meta"),
  function(object, col_subset = NULL) { # ---- dat_template -------------------

    key_cols <- data.table::key(dat(object))

    if (is.null(col_subset)) {
      template <- dat(object)[0,!key_cols, with = FALSE]
    }

    if (!is.null(col_subset)) {
      stopifnot(all(key_cols != col_subset),
                all(col_subset %in% variable_names(object)))

      template <- dat(object)[0,col_subset, with = FALSE]
    }

    return(invisible(template))
  })

#' @param drop_columns A vector indicating columns of the data;
#'     either character or integer.
#' @rdname drop_columns
setMethod(
  f = "drop_columns",
  signature = c("sp_network_meta"),
  function(object, drop_columns) { # ---- drop_columns ------------------------
    dat(object)[ , c(drop_columns) := NULL]
    return(invisible(dat(object)))
  })


#' @export
#' @param new_cols New columns to be added to existing data can be one of;
#'   scalar, vector, data.frame
#' @rdname set_columns
setMethod(
  f = "set_columns",
  signature = c("sp_network_meta"),
  function(object, new_cols) { # ---- set_columns --------------------------------

    new_cols <- data.frame(new_cols,check.names = TRUE)
    assert(nrow(new_cols) == 1 || nrow(new_cols) == nrow(dat(object)),
           "The dimensions of the new data does not match with the existing!")
    dat(object)[ , names(new_cols) := new_cols]

    return(invisible(dat(object)))
  })


#' @rdname variable_names
#' @export
setMethod(
  f = "variable_names",
  signature = c("sp_network_meta"),
  function(object) { # ---- variable_names ------------------------------------
    return(names(dat(object)))
  })

#' @rdname variable_names
#' @export
setReplaceMethod(
  f = "variable_names",
  signature = "sp_network_pair",
  function(object,value) {
    names(dat(object)) <- value
    if (validObject(object))
      return(object)
  })
