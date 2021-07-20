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

  TSS <- crossproduct_mat_list(model_matrices$Y_, Y_wt)

  ## ---- Range of the eigenvalues for neighborhood matrices
  is_spatial <- estim_control[["estimation_method"]] != "ols"
  OW_eigen_range <- DW_eigen_range <- NULL

  if (is_spatial) {
    real_eigen_range <- function(W) {
      if (is.null(W))
        return(NULL)

      max_abs <- RSpectra::eigs(W,1,"LM")$values

      if (Re(max_abs) > 0) {
        max_re <- max_abs
        min_re <- RSpectra::eigs(W,1,"SR")$values
      }

      if (Re(max_abs) < 0) {
        min_re <- max_abs
        max_re <- RSpectra::eigs(W,1,"LR")$values
      }

      return(c(min_re, max_re))
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

  is_2sls <- estim_control[["estimation_method"]] == "s2sls"
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
