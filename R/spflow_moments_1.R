#' @title Compute the principal moments for a spatial flow model
#' @keywords internal
spflow_model_moments <- function(...) {

  moments <- compute_spflow_moments(...)

  error_msg <-
    "The estimation is aborted because the %s variables contain " %p%
    "infinite values or NA's!" %p%
    "\nPlease check that all variables are well defined and that all " %p%
    "tranformations are valid (e.g avoid logarithms of 0)."
  assert(none(is.infinite(moments[["ZZ"]])) & none(is.na(moments[["ZZ"]])),
         error_msg, "explanatory")
  assert(none(is.infinite(moments[["ZY"]])) & none(is.na(moments[["ZY"]])),
         error_msg, "response")

  return(moments)
}

#' @importFrom Matrix nnzero
#' @keywords internal
compute_spflow_moments <- function(
    model_matrices,
    flow_control
) {

  ## ---- define dimensionality of the estimation
  n_o <- flow_control[["mat_nrows"]]
  n_d <- flow_control[["mat_ncols"]]
  N <- flow_control[["mat_npairs"]]


  ## ---- derive moments from the covariates (Z,H)
  UU <- moment_empirical_var(model_matrices,N,n_d,n_o)

  # subset ZZ
  variable_order <- c("const","const_intra","D_","O_","I_","G_")
  is_instrument <- rapply(model_matrices[variable_order],f = attr_inst_status)
  Z_index <- !as.logical(is_instrument)
  ZZ <- UU[Z_index, Z_index]


  ## ---- derive moments from the response (UY, ZY, TSS)
  # ...weighted Y if required
  Y_wt <- model_matrices$weights %|!|%
    lapply(model_matrices$Y_,"*",model_matrices$weights)
  UY <- Y_wt %||% model_matrices$Y_
  UY <- lapply(UY, "moment_empirical_covar", model_matrices)
  UY <- Reduce("cbind", x = UY,init = matrix(nrow = nrow(UU),ncol = 0))
  dimnames(UY) <- list(rownames(UU), names(model_matrices$Y_))

  ZY <- UY[Z_index, , drop = FALSE]

  TSS <- crossproduct_mat_list(model_matrices$Y_, Y_wt)

  is_2sls <- flow_control[["estimation_method"]] == "s2sls"
  model_moments <- compact(list(
    "n_d"       = n_d,
    "n_o"       = n_o,
    "N"         = N,
    "UU"        = UU %T% is_2sls,   # only for s2sls
    "ZZ"        = ZZ,
    "UY"        = UY %T% is_2sls,   # only for s2sls
    "ZY"        = ZY,
    "TSS"       = TSS))

  # (if required)
  # Functions to validate the parameter space
  # and to calculate the log determinant
  nbfunctions <- spflow_nbfunctions(
    OW = model_matrices[["OW"]],
    DW = model_matrices[["DW"]],
    flow_control = flow_control,
    flow_indicator = model_matrices[["flow_indicator"]])

  return(c(model_moments, nbfunctions))
}


#' @keywords internal
spflow_nbfunctions <- function(
    OW,
    DW,
    flow_control,
    flow_indicator) {


  if (flow_control[["estimation_method"]] == "ols")
    return(NULL)

  nbfunctions <- named_list(c("logdet_calculator", "pspace_validator"))
  if (flow_control[["estimation_method"]] %in% c("mle", "mcmc")) {
    nbfunctions[["logdet_calculator"]] <- derive_logdet_calculator(
      OW = OW,
      DW = DW,
      model = flow_control[["model"]],
      n_o = flow_control[["mat_ncols"]],
      n_d = flow_control[["mat_nrows"]],
      approx_order = flow_control[["logdet_approx_order"]],
      is_cartesian = is.null(flow_indicator) | flow_control[["logdet_simplify2cartesian"]],
      flow_indicator = flow_indicator)
  }



  # TODO finish the parameter space...
  # nbfunctions[["pspace_validator"]] <- derive_param_space_validator(
  #   OW = ,DW = ,model = ,)
  # )

  return(compact(nbfunctions))
}
