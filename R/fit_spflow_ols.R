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
  results_df <- create_results(
    est = as.vector(delta),
    sd = sd_delta,
    df = N - length(delta) - 1)

  LL <- - (N/2)*log(2*pi) + (N/2)*log(N) - N/2 - N * log(RSS) / 2
  estimation_diagnostics <- list(
    "sd_error" = sqrt(sigma2),
    "varcov" = varcov,
    "ll" = LL,
    "AIC" = -2 * LL + 2 * length(delta),
    "BIC" = -2 * LL + log(N) * length(delta),
    "model_coherence" = "Validated")

  if (isTRUE(estimation_control[["track_condition_numbers"]]))
    estimation_diagnostics <- c(
      estimation_diagnostics,
      "rcond" = rcond(ZZ))

  estimation_results <- spflow_model(
    estimation_results = results_df,
    estimation_control = estimation_control,
    estimation_diagnostics = estimation_diagnostics)

  return(estimation_results)
}

