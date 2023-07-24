#' @keywords internal
spflow_mle <- function(
  ZZ,
  ZY,
  TSS,
  N,
  n_d,
  n_o,
  TCORR,
  logdet_calculator,
  pspace_validator,
  estimation_control) {

  model <- estimation_control[["model"]]
  hessian_method <- estimation_control[["mle_hessian_method"]]

  # compute the decomposed coefficients to obtain the decomposed RSS
  delta_t <- qr.coef(qr(ZZ), ZY)
  dd <- !is.na(delta_t[,1])
  RSS <- TSS - crossprod(ZY[dd,,drop=FALSE],delta_t[dd,,drop=FALSE])
  assert(all(dd) || estimation_control[["allow_singular"]],
         "Encountered singular fit!")

  # initialization for optimization
  optim_count <- 1
  optim_limit <- estimation_control[["mle_optim_limit"]]
  nb_rho <- ncol(ZY) - 1
  optim_part_LL <- function(rho) {
    tau <- c(1, -rho)
    rss_part <- N * log(tau %*% RSS %*% tau) / 2
    return(rss_part - logdet_calculator(rho))
  }
  if (model == "model_8") {
    nb_rho <- 2
    rho_tmp <- draw_initial_guess(nb_rho)
    optim_part_LL <- function(rho) {
      rho <- c(rho, -prod(rho))
      tau <- c(1, -rho)
      rss_part <- N * log(tau %*% RSS %*% tau) / 2
      return(rss_part - logdet_calculator(rho))
      }
  }
  rho_tmp <- draw_initial_guess(nb_rho)
  optim_results <- structure(rho_tmp,class = "try-error")

  while (is(optim_results,"try-error") & (optim_count < optim_limit)) {

    optim_results <- try(silent = TRUE, expr = {
      optim(rho_tmp, optim_part_LL, gr = NULL, method = "L-BFGS-B",
            lower = rep(-0.999, nb_rho), upper = rep(0.999, nb_rho),
            hessian = TRUE)})
    optim_count <- optim_count + 1

    # new guess for next iteration
    rho_tmp <- draw_initial_guess(nb_rho)
  }

  assert(optim_count < optim_limit,
         "Optimization of the likelihood failed to converge within %s tries.",
         optim_limit)

  # coeffcients
  rho <- optim_results$par
  if (model == "model_8") rho <- c(rho, -prod(rho))
  rho <- lookup(rho, define_spatial_lag_params(model))
  tau <- c(1, -rho)
  delta <- (delta_t %*% tau)[,1]
  mu <- c(rho, delta)

  # inference
  sigma2 <-  as.numeric(1 / N * (tau %*% RSS %*% tau))

  hessian_inputs <- list(
    "ZZ"  = ZZ[dd, dd, drop = FALSE],
    "ZY"  = ZY[dd,   , drop = FALSE],
    "TSS" = TSS, "N" = N,
    "rho" = rho, "delta" = delta[dd], "sigma2" = sigma2)


  if ( hessian_method == "mixed" ) {
    mixed_specific <- list("numerical_hess" = -optim_results$hessian)
    hessian_inputs <- c(hessian_inputs,mixed_specific)
  }

  if ( hessian_method == "f2" ) {
    assert(model != "model_8", "F2 hessian approximation cannot be used for model_8!")
    f2_specific <- list("delta_t" = delta_t, "calc_log_det" = logdet_calculator)
    hessian_inputs <- c(hessian_inputs,f2_specific)
  }

  hessian <- spflow_hessian(hessian_method, hessian_inputs)
  varcov <- chol2inv(chol(-hessian))
  if (model == "model_8") { # derive Var(rho_w) with delta method
    del <- rbind(diag(2),-rho[2:1])
    del <- block_diag(del, diag(length(delta[dd]) + 1))
    varcov <- del %*% varcov %*% t(del)
  }
  if (any(is.na(mu))){
    vv <- length(rho) + length(delta) + length("sigma2")
    vv <- matrix(nrow = vv, ncol = vv)
    v_obs <- c(!is.na(mu), TRUE) # (sigma)
    vv[v_obs, v_obs] <- varcov
    varcov <- vv
  }
  dimnames(varcov) <- list(c(names(rho),names(delta),"sigma2"))[c(1,1)]
  sd_mu <- mu
  sd_mu[colnames(varcov)] <- sqrt(diag(varcov))

  ll_const_part <- -(N/2)*log(2*pi) + (N/2)*log(N) - N/2
  ll_partial <- -optim_results$value
  ll <- ll_partial + ll_const_part

  id_sigma <- length(sd_mu)
  k <- ncol(varcov)
  results_df <- create_results(est = mu, sd = sd_mu[-id_sigma], df = N - k)

  estimation_diagnostics <- list(
    "sd_error" = sqrt(sigma2),
    "varcov" = varcov,
    "ll" = ll,
    "AIC" = -2 * ll + 2 * k,
    "BIC" = -2 * ll + log(N) * k,
    "model_coherence" = ifelse(pspace_validator(rho), "Validated", "Unknown"))
  if (isTRUE(estimation_control[["track_condition_numbers"]]))
    estimation_diagnostics <- c(estimation_diagnostics, "rcond" = rcond(ZZ))

  estimation_results <- spflow_model(
    estimation_results = results_df,
    estimation_control = estimation_control,
    estimation_diagnostics = estimation_diagnostics)

  return(estimation_results)
}

#' @keywords internal
draw_initial_guess <- function(n_param) {
  init <- runif(n_param)

  if (n_param > 1) {
    norm <- (0.7 / 3) * n_param
    init <- norm * init / sum(init)
  }
  return(init)
}
