#' @title Compute the principal moments for a spatial flow model
#' @importFrom Matrix nnzero
#' @keywords internal
compute_spflow_moments <- function(
    spflow_matrices,
    n_o,
    n_d,
    N,
    wt,
    na_rm = FALSE) {




  ## ---- derive moments from the covariates (Z,H)
  UU <- spflow_moment_var(spflow_matrices, wt, N, n_d, n_o)
  assert(is.null(wt) || nnzero(wt) > ncol(UU), "
         Estimation aborted!
         There are too few complete observations to identifiy the parameters.")

  # subset ZZ
  variable_order <- c("CONST", "D_", "O_", "I_", "G_")
  is_instrument <- rapply(spflow_matrices[variable_order],f = attr_inst_status)
  Z_index <- !as.logical(is_instrument)
  ZZ <- UU[Z_index, Z_index ,drop = FALSE]


  ## ---- derive moments from the response (UY, ZY, TSS)
  # ...weighted Y if required
  Y_wt <- wt %|!|% lapply(spflow_matrices$Y_,"*",wt)
  UY <- Y_wt %||% spflow_matrices$Y_
  UY <- lapply(UY, "spflow_moment_cov", spflow_matrices)
  UY <- Reduce("cbind", x = UY,init = matrix(nrow = nrow(UU),ncol = 0))
  dimnames(UY) <- list(rownames(UU), names(spflow_matrices$Y_))

  ZY <- UY[Z_index, , drop = FALSE]
  TSS <- crossproduct_mat_list(spflow_matrices$Y_, Y_wt)

  # covariance matrix
  TCORR <- rbind(cbind(UU,UY), cbind(t(UY), TSS))
  TCORR <- TCORR - (outer(TCORR[1,], TCORR[1,])/N)
  TCORR <- TCORR / outer(sqrt(diag(TCORR)), sqrt(diag(TCORR)))
  diag(TCORR[-1,-1]) <- 1

  model_moments <- compact(list(
    "n_d"   = n_d,
    "n_o"   = n_o,
    "N"     = N,
    "UU"    = UU,
    "ZZ"    = ZZ,
    "UY"    = UY,
    "ZY"    = ZY,
    "TSS"   = TSS,
    "TCORR" = TCORR))

  if (!na_rm) {
    error_msg <- "
    The estimation is aborted because the %s variables contain
    infinite values or NA's!
    <br>Check that all variables are well defined and that all
    tranformations are valid (e.g. avoid logarithms of 0)."
    assert(all(is.finite(UU)),error_msg, "explanatory")
    assert(all(is.finite(UY)), error_msg, "response")
  }

  return(model_moments)
}
