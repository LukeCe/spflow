#' @keywords internal
spflow_model_moments <- function(...) {

  moments <- compute_spflow_moments(...)

  error_msg <-
    "The estimation is aborted because the %s variables contain " %p%
    "infinite values!" %p%
    "\nPlease check that all variables are well defined and that all " %p%
    "tranformations are valid (e.g avoid logarithms of 0)."
  assert(none(is.infinite(moments$ZZ)), error_msg, "explanatory")
  assert(none(is.infinite(moments$ZY)), error_msg, "response")

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
  HH <- moment_empirical_var(model_matrices,N,n_d,n_o)

  # subset ZZ
  variable_order <- c("constants","D_","O_","I_","G_")
  Z_index <- !rapply(model_matrices[variable_order],f = attr_inst_status)
  ZZ <- HH[Z_index, Z_index]


  ## ---- derive moments from the response (HY, ZY, TSS)
  # ...weighted Y if required
  Y_wt <- model_matrices$weights %|!|%
    lapply(model_matrices$Y_,"*",model_matrices$weights)
  HY <- Y_wt %||% model_matrices$Y_
  HY <- lapply(HY, "moment_empirical_covar", model_matrices)
  HY <- Reduce("cbind", x = HY,init = matrix(nrow = nrow(HH),ncol = 0))
  dimnames(HY) <- list(rownames(HH), names(model_matrices$Y_))

  ZY <- HY[Z_index, , drop = FALSE]

  # ... TSS (dim = 1, for GMM | dim = rho + 1, for LL)
  # total sum of squares is different for GMM and likelihood based estimators
  # because the lagged flows are considered as endogenous regresses and not
  # as additional dependent variable
  is_twosls <- estim_control[["estimation_method"]] == "s2sls"
  y_index <- if (is_twosls) 1L else seq_len(ncol(ZY))
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
    "HH"        = HH %T% is_twosls,
    "ZZ"        = ZZ,
    "HY"        = HY %T% is_twosls,
    "ZY"        = ZY,
    "TSS"       = TSS,
    "OW_traces" = OW_traces,
    "DW_traces" = DW_traces))

  return(model_moments)
}
