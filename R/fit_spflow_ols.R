#' @keywords internal
spflow_ols <- function(ZZ,ZY,TSS,N,TCORR,estimation_control) {

  delta <- solve_savely(ZZ, ZY, TCORR)

  # standard errors
  ESS <- crossprod(ZY, delta)
  RSS <- TSS - ESS
  sigma2 <- sum(RSS)/N

  # variance covariance matrix of the parameters
  varcov <- sigma2 * solve(ZZ)
  sd_delta <- sqrt(diag(varcov))
  results_df <- create_results(est = as.vector(delta), sd = sd_delta)

  estimation_diagnostics <- list(
    "sd_error" = sqrt(sigma2),
    "varcov" = varcov)
  if (isTRUE(estimation_control[["track_condition_numbers"]]))
    estimation_diagnostics <- c(estimation_diagnostics, "rcond" = rcond(ZZ))

  estimation_results <- spflow_model(
    estimation_results = results_df,
    estimation_control = estimation_control,
    estimation_diagnostics = estimation_diagnostics)

  return(estimation_results)
}

