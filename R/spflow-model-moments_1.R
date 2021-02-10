#' @keywords internal
spflow_model_moments <- function(...) {

  model_moments <- spflow_model_moments_mat(...)

  ## ... verify the model moments
  infinities_error_template <-
    "The estimation is aborted because the %s variables contain " %p%
    "infinite values!" %p%
    "\nPlease check that all variables are well defined and that all " %p%
    "tranformations are valid (e.g avoid logarithms of 0)."

  assert(all(!is.infinite(model_moments$ZZ)),
         sprintf(infinities_error_template, "explanatory"))

  assert(all(!is.infinite(model_moments$ZY)),
         sprintf(infinities_error_template, "response"))


  return(model_moments)

}

#' @importFrom Matrix nnzero
#' @keywords internal
spflow_model_moments_mat <- function(
  model_matrices,
  estimator
) {

  ## ---- define dimensionality of the estimation
  n_o <- nrow(model_matrices$Y_[[1]])
  n_d <- ncol(model_matrices$Y_[[1]])
  N <- (n_o * n_d) %T% is.null(model_matrices$weights) # full   case
  N <- N %||% nnzero(model_matrices$weights)           # sparse case


  ## ---- derive moments from the covariates (Z,H)
  HH <- moment_empirical_var(model_matrices,N,n_d,n_o)

  # subset ZZ
  variable_order <- c("constants","D_","O_","I_","G_")
  Z_index <- !rapply(model_matrices[variable_order],f = get_instrument_status)
  ZZ <- HH[Z_index, Z_index]


  ## ---- derive moments from the response (HY, ZY, TSS)
  # ...weighted Y if required
  Y_wt <- model_matrices$weights %|!|%
    lapply(model_matrices$Y_,"*",model_matrices$weights)
  HY <- (Y_wt %||% model_matrices$Y_) %>%
    lapply("moment_empirical_covar",model_matrices) %>%
    Reduce(cbind, x = .,init = matrix(nrow = nrow(HH),ncol = 0))
  ZY <- HY[Z_index, , drop = FALSE]

  # ... TSS (dim = 1, for GMM | dim = rho + 1, for LL)
  # total sum of squares is different for GMM and likelihood based estimators
  # because the lagged flows are considered as endogenous regresses and not
  # as additional dependent variable
  is_GMM_estimator <- estimator %in% c("s2sls","ols")
  y_index <- if (is_GMM_estimator) 1L else seq_len(ncol(ZY))
  TSS <- crossproduct_mat_list(model_matrices$Y_[y_index],
                               Y_wt[y_index])

  ## ---- Likelihood moments (trace sequence of the weight matrix)
  # sequence of traces to approximate the log-determinant
  # is only relevant to likelihood based estimators
  approximate_order <- 10
  # TODO the trace approximation should be directly on the flow neighborhood
  # .... this requires to refine the likelihood evaluation change with [p3]
  OW_traces <- model_matrices$OW %T% !is_GMM_estimator
  OW_traces <- OW_traces %|!|% trace_sequence(OW_traces, approximate_order)
  DW_traces <- model_matrices$DW %T% !is_GMM_estimator
  DW_traces <- DW_traces %|!|% trace_sequence(DW_traces, approximate_order)

  model_moments <- list(
    "n_d"       = n_d,
    "n_o"       = n_o,
    "N"         = N,
    "HH"        = HH %T% is_GMM_estimator,
    "ZZ"        = ZZ,
    "HY"        = HY %T% is_GMM_estimator,
    "ZY"        = ZY,
    "TSS"       = TSS,
    "OW_traces" = OW_traces,
    "DW_traces" = DW_traces) %>% compact()

  return(model_moments)
}