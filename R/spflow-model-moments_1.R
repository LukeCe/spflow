#' @keywords internal
spflow_model_moments <- function(...) {

  moments <- compute_spflow_moments(...)

  error_msg <-
    "The estimation is aborted because the %s variables contain " %p%
    "infinite values!" %p%
    "\nPlease check that all variables are well defined and that all " %p%
    "tranformations are valid (e.g avoid logarithms of 0)."
  assert(none(is.infinite(moments[["ZZ"]])), error_msg, "explanatory")
  assert(none(is.infinite(moments[["ZY"]])), error_msg, "response")

  return(moments)
}

#' @importFrom Matrix nnzero
#' @keywords internal
compute_spflow_moments <- function(
  model_matrices,
  estim_control
) {

  ## ---- define dimensionality of the estimation
  n_o <- estim_control[["mat_nrows"]]
  n_d <- estim_control[["mat_ncols"]]
  N <- estim_control[["mat_npairs"]]


  ## ---- derive moments from the covariates (Z,H)
  UU <- moment_empirical_var(model_matrices,N,n_d,n_o)

  # subset ZZ
  variable_order <- c("constants","D_","O_","I_","G_")
  Z_index <- !rapply(model_matrices[variable_order],f = attr_inst_status)
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

  # ... TSS (dim = 1, for GMM | dim = rho + 1, for LL)
  # total sum of squares is different for GMM and likelihood based estimators
  # because the lagged flows are considered as endogenous regresses and not
  # as additional dependent variable
  is_2sls <- estim_control[["estimation_method"]] == "s2sls"
  y_index <- if (is_2sls) 1L else seq_len(ncol(ZY))
  TSS <- crossproduct_mat_list(model_matrices$Y_[y_index], Y_wt[y_index])

  ## ---- Likelihood moments (trace sequence of the weight matrix)
  OW_traces <- DW_traces <- NULL
  approximate_order <- 10
  if (estim_control[["estimation_method"]] %in% c("mle", "mcmc")) {
    if (!is.null(model_matrices[["OW"]]))
      OW_traces <- trace_sequence(model_matrices[["OW"]], approximate_order)
    if (!is.null(model_matrices[["DW"]]))
      DW_traces <- trace_sequence(model_matrices[["DW"]], approximate_order)
  }

  model_moments <- compact(list(
    "n_d"       = n_d,
    "n_o"       = n_o,
    "N"         = N,
    "UU"        = UU %T% is_2sls,
    "ZZ"        = ZZ,
    "UY"        = UY %T% is_2sls,
    "ZY"        = ZY,
    "TSS"       = TSS,
    "OW_traces" = OW_traces,
    "DW_traces" = DW_traces))

  return(model_moments)
}
