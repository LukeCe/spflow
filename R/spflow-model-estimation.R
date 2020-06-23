spflow_model_estimation <- function(
  model_moments,
  flow_control) {

  estimator <- flow_control$estimation_method

  estimation_results <- switch(estimator,
    "s2sls" = {
      spflow_s2sls(
        HH  = model_moments$HH,
        HY  = model_moments$HZ,
        ZZ  = model_moments$ZZ,
        ZY  = model_moments$ZY,
        TSS = model_moments$TSS,
        N   = model_moments$N
      )},
    "mle" = {
      spflow_mle(
        ZZ    = model_moments$ZZ,
        ZY    = model_moments$ZY,
        TSS   = model_moments$TSS,
        N     = model_moments$N,
        n_d   = model_moments$n_d,
        n_o   = model_moments$n_o,
        OW_traces = model_moments$OW_traces,
        DW_traces = model_moments$DW_traces,
        model = flow_control$model,
        hessian_method = flow_control$hessian_method)}
  )

  return(estimation_results)

}

spflow_s2sls <- function(HH,HY,ZZ,ZY,TSS,N) {

  # number of auto-regressive parameters and model coefficients and total
  nb_rho <- ncol(HY) - 1
  nb_delta <- ncol(ZZ)
  size_mu <- nb_rho + nb_delta

  # generate the moments of the second stage
  stage2_ZZ <- matrix(nrow = size_mu, ncol = size_mu)
  stage2_ZY <- vector(length = size_mu)

  index_rho <- seq_len(nb_rho)
  index_delta <- seq_len(nb_delta) + nb_rho

  # fill four blocks of the second stage variance moment
  HJ <- HY[,-1]
  ZJ <- ZY[,-1]
  JJ <- crossprod(HJ,solve(HH,HJ))
  stage2_ZZ[index_rho,index_rho] <- JJ
  stage2_ZZ[index_rho,index_delta] <- t(ZJ)
  stage2_ZZ[index_delta,index_rho] <- ZJ
  stage2_ZZ[index_delta,index_delta] <- ZZ

  # fill two blocks of the second stage covariance moment
  stage2_ZY[index_rho] <- crossprod(HJ,solve(HH,HY[,1]))
  stage2_ZY[index_delta] <- ZY[,1]

  # parameters
  mu <- solve(stage2_ZZ,stage2_ZY)

  # standard errors
  ESS <- crossprod(stage2_ZY, mu)
  RSS <- TSS - ESS
  sigma2 <- sum(RSS)/N

  # variance covariance matrix of the parameters
  varcov <- sigma2 * solve(stage2_ZZ)
  sd_mu <- sqrt(diag(varcov))

  results_df <- data.frame(
    "est" = mu,
    "sd" = sd_mu)

  results_df$"t.stat" <- results_df$est / results_df$sd
  results_df$"p.value" <- 1 - pt(abs(results_df$est / results_df$sd), N - nb_delta)

  estimation_results <- spflow_model(
    results_df = results_df,
    varcov = varcov,
    sd_error = sqrt(sigma2),
    N = N,
    method = "s2sls",
    formulation = "matrix")

  return(estimation_results)
}

spflow_model <- function(
  results_df,
  varcov,
  sd_error,
  N,
  method,
  formulation,
  ...) {


    c(list("results" = results_df,
           "varcov" = varcov,
           "sd" = sd_error,
           "N" = N,
           "method" = method,
           "residuals" = NULL,
           "fitted" = NULL,
           "data" = NULL,
           "formulation" = formulation),
      list(...)
      ) %>%
    structure(class = "spflow_model")
}

