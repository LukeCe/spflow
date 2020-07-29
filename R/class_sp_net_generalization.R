#' @include class_sp-network.R class_sp-network-pair.R class_virtual.R utils.R

setClassUnion("sp_net_generalization" ,c("sp_network", "sp_network_pair"))

# ---- methods ----------------------------------------------------------------

#' @export
#' @param new_cols New columns to be added to existing data can be one of;
#'   scalar, vecotor, data.frame
#' @rdname set_columns
setMethod(
  f = "set_columns",
  signature = c("sp_net_generalization"),
  function(object, new_cols) { # ---- set_columns --------------------------------

    new_cols <- data.frame(new_cols,check.names = TRUE)
    assert(nrow(new_cols) == 1 || nrow(new_cols) == nrow(dat(object)),
           "The dimensions of the new data does not match with the existing!")
    dat(object)[ , names(new_cols) := new_cols]

    return(invisible(dat(object)))
  })

#' @param drop_columns A vector indicating columns of the data;
#'     either character or integer.
#' @rdname drop_columns
setMethod(
  f = "drop_columns",
  signature = c("sp_net_generalization"),
  function(object, drop_columns) { # ---- drop_columns ------------------------
    dat(object)[ , c(drop_columns) := NULL]
    return(invisible(dat(object)))
  })


#' @rdname variable_names
#' @export
setMethod(
  f = "variable_names",
  signature = c("sp_net_generalization"),
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
