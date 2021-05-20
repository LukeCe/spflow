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

  ## ---- Range of the eigenvalues for neighborhood matrices
  is_spatial <- estim_control[["estimation_method"]] != "ols"
  OW_eigen_range <- DW_eigen_range <- NULL
  # TODO implement parameter space checks
  if (is_spatial & FALSE) {
    real_eigen_range <- function(W) {
      if (is.null(W))
        return(NULL)

      max_re <- RSpectra::eigs(W,1,"LR")$values
      max_re <- abs(max_re) * sign(Re(max_re))
      min_re <- RSpectra::eigs(W,1,"SR")$values
      min_re <- abs(min_re) * sign(Re(min_re))
      range_re <- c(max_re, min_re)

      # check if the largest absolute eigenvalue is complex
      # no exact tests are possible in this case
      max_abs <- RSpectra::eigs(W,1,"LM")$values
      if (Im(max_abs) == 0)
        return(range_re)

      return("no exact solution")
      }

    OW_eigen_range <- DW_eigen_range <-
      real_eigen_range(model_matrices[["OW"]])

    if (!estim_control[["mat_within"]])
      DW_eigen_range <- real_eigen_range(model_matrices[["DW"]])

    }

  ## ---- Likelihood moments (trace sequence of the weight matrix)
  OW_traces <- DW_traces <- NULL
  uses_loglik <- estim_control[["estimation_method"]] %in% c("mle", "mcmc")

  if (uses_loglik) {
    approx_order <- estim_control[["loglik_det_aprox_order"]]
    OW_traces <- DW_traces <-
      trace_sequence(model_matrices[["OW"]], approx_order)

    if (!estim_control[["mat_within"]])
      DW_traces <- trace_sequence(model_matrices[["DW"]],approx_order)
  }

  model_moments <- compact(list(
    "n_d"       = n_d,
    "n_o"       = n_o,
    "N"         = N,
    "UU"        = UU %T% is_2sls,   # only for s2sls
    "ZZ"        = ZZ,
    "UY"        = UY %T% is_2sls,   # only for s2sls
    "ZY"        = ZY,
    "TSS"       = TSS,
    "DW_eigen_range" = DW_eigen_range, # not for ols
    "OW_eigen_range" = OW_eigen_range, # not for ols
    "DW_traces" = DW_traces, # only for mle and mcmc
    "OW_traces" = OW_traces  # only for mle and mcmc
    ))

  return(model_moments)
}
