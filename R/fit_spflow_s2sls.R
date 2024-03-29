#' @keywords internal
spflow_s2sls <- function(UU,UY,ZZ,ZY,TSS,N,TCORR,pspace_validator,estimation_control) {

  # number of auto-regressive parameters and model coefficients and total
  nb_rho <- ncol(UY) - 1
  nb_delta <- ncol(ZZ)
  size_mu <- nb_rho + nb_delta
  rho_names <- define_spatial_lag_params(estimation_control[["model"]])
  mu_names <- c(rho_names, colnames(ZZ))

  # generate the moments of the second stage
  stage2_ZZ <- matrix(
    nrow = size_mu, ncol = size_mu,
    dimnames = list(mu_names, mu_names))
  stage2_ZY <- lookup(0, mu_names)

  index_rho <- seq_len(nb_rho)
  index_delta <- seq_len(nb_delta) + nb_rho

  qr_UU <- qr(UU)
  assert(qr_UU[["rank"]] == ncol(UU) | estimation_control[["allow_singular"]],
         "Encountered singular fit!")

  # fill four blocks of the second stage variance moment
  UL <- UY[,-1]
  ZL <- ZY[,-1]
  LL_hat <- crossprod(UL, drop_na(qr.coef(qr_UU, UL)))
  stage2_ZZ[index_rho,index_rho] <- LL_hat
  stage2_ZZ[index_rho,index_delta] <- t(ZL)
  stage2_ZZ[index_delta,index_rho] <- ZL
  stage2_ZZ[index_delta,index_delta] <- ZZ

  # fill two blocks of the second stage covariance moment
  stage2_ZY[index_rho] <- crossprod(UL,drop_na(qr.coef(qr_UU, UY[,1])))
  stage2_ZY[index_delta] <- ZY[,1]

  # parameters
  mu <- qr.coef(qr(stage2_ZZ),stage2_ZY)
  dd <- !is.na(mu)
  assert(all(dd) | estimation_control[["allow_singular"]],
         "Encountered singular fit!")

  # standard errors (stage 3)
  stage3_ZZ <- stage2_ZZ
  stage3_ZY <- stage2_ZY
  stage3_ZZ[index_rho,index_rho] <- TSS[index_rho + 1,index_rho + 1]
  stage3_ZY[index_rho] <-  TSS[1, index_rho + 1]

  dd_mu <- drop_na(mu)
  RSS <- TSS[1,1] - 2*stage3_ZY %*% dd_mu + dd_mu %*% stage3_ZZ %*% dd_mu
  sigma2 <- sum(RSS)/N

  # variance covariance matrix of the parameters
  varcov <- sigma2 * chol2inv(chol(stage2_ZZ[dd,dd]))
  dimnames(varcov) <- list(names(mu[dd]))[c(1,1)]
  sd_mu <- mu
  sd_mu[colnames(varcov)] <- sqrt(diag(varcov))

  results_df <- create_results(
    "est" = mu,
    "sd" = sd_mu,
    df = N - ncol(varcov))

  rho <- mu[index_rho]
  estimation_diagnostics <- list(
    "sd_error" = sqrt(sigma2),
    "varcov" = varcov,
    "model_coherence" = ifelse(pspace_validator(rho), "Validated", "Unknown"))
  if (isTRUE(estimation_control[["track_condition_numbers"]]))
    estimation_diagnostics <- c(estimation_diagnostics, "rcond" = rcond(ZZ), "rcond_stage1" = rcond(UU), "rcond_stage2" = rcond(stage2_ZZ))

  estimation_results <- spflow_model(
    estimation_results = results_df,
    estimation_control = estimation_control,
    estimation_diagnostics = estimation_diagnostics)

  return(estimation_results)
}



