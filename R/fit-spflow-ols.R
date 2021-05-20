#' @keywords internal
spflow_ols <- function(ZZ,ZY,TSS,N,flow_control) {

  delta <- solve(ZZ,ZY)

  # standard errors
  ESS <- crossprod(ZY, delta)
  RSS <- TSS - ESS
  sigma2 <- sum(RSS)/N

  # variance covariance matrix of the parameters
  varcov <- sigma2 * solve(ZZ)
  sd_delta <- sqrt(diag(varcov))

  results_df <- data.frame(
    "est" = as.vector(delta),
    "sd" = sd_delta)

  estimation_results <- spflow_model(
    varcov = varcov,
    estimation_results = results_df,
    estimation_control = flow_control,
    sd_error = sqrt(sigma2),
    N = N)

  return(estimation_results)
}

