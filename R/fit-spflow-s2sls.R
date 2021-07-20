#' @keywords internal
spflow_s2sls <- function(UU,UY,ZZ,ZY,TSS,N,flow_control) {

  # number of auto-regressive parameters and model coefficients and total
  nb_rho <- ncol(UY) - 1
  nb_delta <- ncol(ZZ)
  size_mu <- nb_rho + nb_delta
  mu_names <- c(define_spatial_lag_params(flow_control[["model"]]),
                colnames(ZZ))

  # generate the moments of the second stage
  stage2_ZZ <- matrix(
    nrow = size_mu, ncol = size_mu,
    dimnames = list(mu_names, mu_names))
  stage2_ZY <- lookup(0, mu_names)

  index_rho <- seq_len(nb_rho)
  index_delta <- seq_len(nb_delta) + nb_rho

  # fill four blocks of the second stage variance moment
  UL <- UY[,-1]
  ZL <- ZY[,-1]
  LL_hat <- crossprod(UL,solve(UU,UL))
  stage2_ZZ[index_rho,index_rho] <- LL_hat
  stage2_ZZ[index_rho,index_delta] <- t(ZL)
  stage2_ZZ[index_delta,index_rho] <- ZL
  stage2_ZZ[index_delta,index_delta] <- ZZ

  # fill two blocks of the second stage covariance moment
  stage2_ZY[index_rho] <- crossprod(UL,solve(UU,UY[,1]))
  stage2_ZY[index_delta] <- ZY[,1]

  # parameters
  mu <- solve(stage2_ZZ,stage2_ZY)

  # standard errors (stage 3)
  stage3_ZZ <- stage2_ZZ
  stage3_ZY <- stage2_ZY
  stage3_ZZ[index_rho,index_rho] <- TSS[index_rho + 1,index_rho + 1]
  stage3_ZY[index_rho] <-  TSS[1, index_rho + 1]

  RSS <- TSS[1,1] - 2*stage3_ZY %*% mu + mu %*% stage3_ZZ %*% mu
  sigma2 <- sum(RSS)/N

  # variance covariance matrix of the parameters
  varcov <- sigma2 * solve(stage2_ZZ)
  sd_mu <- sqrt(diag(varcov))

  results_df <- data.frame(
    "est" = mu,
    "sd" = sd_mu)

  estimation_results <- spflow_model(
    varcov = varcov,
    estimation_results = results_df,
    estimation_control = flow_control,
    sd_error = sqrt(sigma2),
    N = N)

  return(estimation_results)
}



