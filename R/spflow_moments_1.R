#' @title Compute the principal moments for a spatial flow model
#' @importFrom Matrix nnzero
#' @keywords internal
compute_spflow_moments <- function(
    model_matrices,
    flow_control,
    ignore_na = FALSE) {

  ## ---- define dimensions of the estimation
  flow_dims <- dim(model_matrices[["Y_"]][[1]])
  n_d <- flow_dims[1]
  n_o <- flow_dims[2]
  N <- prod(flow_dims)

  if (!is.null(model_matrices[["flow_indicator"]]))
    N <- nnzero(model_matrices[["flow_indicator"]])

  ## ---- derive moments from the covariates (Z,H)
  UU <- moment_empirical_var(model_matrices,N,n_d,n_o)

  # subset ZZ
  variable_order <- c("const","const_intra","D_","O_","I_","G_")
  is_instrument <- rapply(model_matrices[variable_order],f = attr_inst_status)
  Z_index <- !as.logical(is_instrument)
  ZZ <- UU[Z_index, Z_index ,drop = FALSE]


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

  if (!ignore_na) {
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
