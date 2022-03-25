#' @keywords internal
spflow_ols <- function(ZZ,ZY,TSS,N,TCORR,flow_control) {

  delta <- solve_savely(ZZ, ZY, TCORR)

  # standard errors
  ESS <- crossprod(ZY, delta)
  RSS <- TSS - ESS
  sigma2 <- sum(RSS)/N

  # variance covariance matrix of the parameters
  varcov <- sigma2 * solve(ZZ)
  sd_delta <- sqrt(diag(varcov))

  results_df <- create_results(
    est = as.vector(delta),
    sd = sd_delta)

  diagnostics <- NULL
  if (isTRUE(flow_control[["track_condition_numbers"]]))
    diagnostics <- list("rcond" = rcond(ZZ))


  estimation_results <- spflow_model(
    varcov = varcov,
    estimation_results = results_df,
    flow_control = flow_control,
    sd_error = sqrt(sigma2),
    N = N,
    fit_diagnostics = diagnostics)

  return(estimation_results)
}

