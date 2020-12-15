#' @keywords internal
spflow_mle <- function(ZZ,ZY,TSS,N,n_d,n_o,DW_traces,OW_traces,
                       flow_control) {

  model <- flow_control$model
  hessian_method <- flow_control$hessian_method

  # compute the decomposed coefficients to obtain the decomposed RSS
  delta_t <- solve(ZZ,ZY)
  RSS <- TSS - crossprod(ZY,delta_t)

  ## OPTIMIZE the concentrated likelihood ----

  # initialization
  nb_rho <- ncol(ZY) - 1
  rho_tmp <- draw_initial_guess(nb_rho)
  optim_results <- structure(rho_tmp,class = "try-error")

  # TODO generalize optim -> all model + asymmetric case
  W_traces <- OW_traces
  optim_part_LL <- function(rho) {
    -partial_spflow_loglik(rho,
                           RSS = RSS ,
                           W_traces = W_traces,
                           n_o = n_o,
                           n_d = n_d,
                           model = model)
    }

  optim_count <- 1
  optim_limit <- 100
  while (is(optim_results,"try-error") &
         (optim_count < optim_limit)) {
    optim_results <- try(silent = TRUE, expr = {
      optim(rho_tmp, optim_part_LL, gr = NULL, method = "L-BFGS-B",
            lower = rep(-0.99, nb_rho), upper = rep(0.99, nb_rho),
            hessian = TRUE)})
    optim_count <- optim_count + 1

    # new guess for next iteration
    rho_tmp <- draw_initial_guess(nb_rho)
  }
  if (optim_count == optim_limit) {
    stop("algorithm to find rho did not converge towards a minimum")
  }


  # coeffcients
  rho <- optim_results$par
  tau <- c(1, -rho)
  delta <- delta_t %*% tau

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

  results_df$"t.stat" <- results_df$est / results_df$sd
  results_df$"p.value" <- 1 - pt(q = abs(results_df$est / results_df$sd),
                                 df =  1)

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
