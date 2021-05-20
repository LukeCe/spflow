#' @keywords internal
spflow_mle <- function(
  ZZ,
  ZY,
  TSS,
  N,
  n_d,
  n_o,
  DW_traces,
  OW_traces,
  flow_control) {

  model <- flow_control$model
  hessian_method <- flow_control$mle_hessian_method

  # compute the decomposed coefficients to obtain the decomposed RSS
  delta_t <- solve(ZZ,ZY)
  RSS <- TSS - crossprod(ZY,delta_t)

  ## OPTIMIZE the concentrated likelihood ----
  clalc_log_det <-
    derive_log_det_calculator(OW_traces, DW_traces,  n_o, n_d, model)
  optim_part_LL <- function(rho) {
    tau <- c(1, -rho)

    # invert the signs (minimize the negative)
    rss_part <- N * log(tau %*% RSS %*% tau) / 2
    return(rss_part - clalc_log_det(rho))
    }

  # initialization
  nb_rho <- ncol(ZY) - 1
  rho_tmp <- draw_initial_guess(nb_rho)
  optim_results <- structure(rho_tmp,class = "try-error")
  optim_count <- 1
  optim_limit <- flow_control[["mle_optim_limit"]]

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

  # inference
  sigma2 <-  as.numeric(1 / N * (tau %*% RSS %*% tau))

  hessian_inputs <- collect(c("ZZ","ZY","TSS","rho","delta","sigma2"))

  if ( hessian_method == "mixed" ) {
    mixed_specific <- list("numerical_hess" = -optim_results$hessian, "N" = N)
    hessian_inputs <- c(hessian_inputs,mixed_specific)
  }

  if ( hessian_method == "f2" ) {
    f2_specific <- collect(c("n_o","n_d","delta_t","W_traces","model"))
    hessian_inputs <- c(hessian_inputs,f2_specific)
  }

  hessian <- spflow_hessian(hessian_method, hessian_inputs)

  mu <- c(rho, delta)
  varcov <- -solve(hessian)
  sd_mu <- sqrt(diag(varcov))

  ll_const_part <- -(N/2)*log(2*pi) + (N/2)*log(N) - N/2
  ll_partial <- -optim_results$value
  loglik_value <- ll_partial + ll_const_part

  drop_sigma <- length(sd_mu)
  results_df <- data.frame(
    "est" = mu,
    "sd" = sd_mu[-drop_sigma])

  estimation_results <- spflow_model(
    varcov = varcov,
    ll = loglik_value,
    estimation_results = results_df,
    estimation_control = flow_control,
    sd_error = sqrt(sigma2),
    N = N)

  return(estimation_results)
}

#' @keywords internal
partial_spflow_loglik <- function(rho,RSS,W_traces,n_o,n_d,model) {

  ## the relevant part of the likelihood is composed of ...

  # ... the log determinant of the filter
  det_part <- spflow_logdet(rho,W_traces,n_o,n_d,model)

  # ... and the RSS term
  tau <- c(1, -rho)
  N <- n_o * n_d
  rss_part <- -N * log(tau %*% RSS %*% tau) / 2

  return(det_part + rss_part)
}
