#' @include class_sp-network.R class_sp-network-pair.R class_virtual.R utils.R

setClassUnion("sp_net_generalization"    , c("sp_network", "sp_network_pair"))

# ---- methods ----------------------------------------------------------------
#' @export
#' @rdname set_columns
setMethod(
  f = "set_columns",
  signature = c("sp_net_generalization"),
  function(object, value) { # ---- set_columns --------------------------------

    new_cols <- data.frame(value,check.names = TRUE)
    assert(nrow(new_cols) == 1 || nrow(new_cols) == nrow(dat(object)),
           "The dimensions of the new data does not match with the existing!")
    dat(object)[ , names(new_cols) := new_cols]

    return(invisible(dat(object)))
  })
