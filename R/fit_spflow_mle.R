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
  delta_t <- solve_savely(ZZ, ZY, TCORR)
  RSS <- TSS - crossprod(ZY,delta_t)

  # initialization for optimization
  nb_rho <- ncol(ZY) - 1
  rho_tmp <- draw_initial_guess(nb_rho)
  optim_results <- structure(rho_tmp,class = "try-error")
  optim_count <- 1
  optim_limit <- estimation_control[["mle_optim_limit"]]
  optim_part_LL <- function(rho) {
    tau <- c(1, -rho)
    rss_part <- N * log(tau %*% RSS %*% tau) / 2
    return(rss_part - logdet_calculator(rho))
  }

  while (is(optim_results,"try-error") & (optim_count < optim_limit)) {

    optim_results <- try(silent = TRUE, expr = {
      optim(rho_tmp, optim_part_LL, gr = NULL, method = "L-BFGS-B",
            lower = rep(-0.99, nb_rho), upper = rep(0.99, nb_rho),
            hessian = TRUE)})
    optim_count <- optim_count + 1

    # new guess for next iteration
    rho_tmp <- draw_initial_guess(nb_rho)
  }

  assert(optim_count < optim_limit,
         "Optimization of the likelihood failed to converge within %s tries.",
         optim_limit)

  # coeffcients
  rho <- lookup(optim_results$par, define_spatial_lag_params(model))
  tau <- c(1, -rho)
  delta <- (delta_t %*% tau)[,1]
  mu <- c(rho, delta)

  # inference
  sigma2 <-  as.numeric(1 / N * (tau %*% RSS %*% tau))

  hessian_inputs <- collect(c("ZZ","ZY","TSS","rho","delta","sigma2","N"))

  if ( hessian_method == "mixed" ) {
    mixed_specific <- list("numerical_hess" = -optim_results$hessian)
    hessian_inputs <- c(hessian_inputs,mixed_specific)
  }

  if ( hessian_method == "f2" ) {
    f2_specific <- list("delta_t" = delta_t, "calc_log_det" = logdet_calculator)
    hessian_inputs <- c(hessian_inputs,f2_specific)
  }

  hessian <- spflow_hessian(hessian_method, hessian_inputs)
  varcov <- -solve(hessian)
  sd_mu <- sqrt(diag(varcov))

  ll_const_part <- -(N/2)*log(2*pi) + (N/2)*log(N) - N/2
  ll_partial <- -optim_results$value
  loglik_value <- ll_partial + ll_const_part

  id_sigma <- length(sd_mu)
  results_df <- create_results(est = mu, sd = sd_mu[-id_sigma])

  estimation_diagnostics <- list(
    "sd_error" = sqrt(sigma2),
    "varcov" = varcov,
    "ll" = loglik_value,
    "Model coherence:" = ifelse(pspace_validator(rho), "Validated", "Unknown"))
  if (isTRUE(estimation_control[["track_condition_numbers"]]))
    estimation_diagnostics <- c(estimation_diagnostics, "rcond" = rcond(ZZ))

  estimation_results <- spflow_model(
    estimation_results = results_df,
    estimation_control = estimation_control,
    estimation_diagnostics = estimation_diagnostics)

  return(estimation_results)
}
