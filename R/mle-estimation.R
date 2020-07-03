spflow_mle <- function(ZZ,ZY,TSS,N,n_d,n_o,DW_traces,OW_traces,
                       model,hessian_method) {

  # compute the decomposed coefficients to obtain the decomposed RSS
  delta_t <- solve(ZZ,ZY)
  RSS <- TSS - crossprod(ZY,delta_t)

  ## OPTIMIZE the concentrated likelihood ----

  # initialization
  nb_rho <- ncol(ZY) - 1
  rho_tmp <- draw_inital_guess(nb_rho)
  optim_results <- structure(rho_tmp,class = "try-error")

  # TODO generalize optim -> all model + asymmetric case
  optim_part_LL <- function(rho) {
    -partial_spflow_loglik(rho,
                           RSS = RSS ,
                           W_traces = OW_traces,
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
    rho_tmp <- draw_inital_guess(nb_rho)
  }
  if (optim_count == optim_limit) {
    stop("algorithm to find rho did not converge towards a minimum")
  }


  # coeffiecients
  rho <- optim_results$par
  tau <- c(1, -rho)
  delta <- delta_t %*% tau
  mu <- c(rho, delta)

  # inference
  sigma2 <-  as.numeric(1 / N * (tau %*% RSS %*% tau))

  hessian <- spflow_hessian(
    hessian_method = hessian_method,
    numerical_hess = -optim_results$hessian,
    ZZ = ZZ,
    ZY = ZY,
    TSS = TSS,
    N = N,
    mu = mu,
    sigma2 = sigma2
  )
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
                                 df =  N - length(delta))

  estimation_results <- spflow_model(
    results_df = results_df,
    varcov = varcov,
    sd_error = sqrt(sigma2),
    N = N,
    method = "mle",
    formulation = "matrix",
    ll = loglik_value,
    hessian_method = hessian_method
    )

  return(estimation_results)
}

draw_inital_guess <- function(n_param) {
  init <- runif(n_param)

  if (n_param > 1) {
    norm <- (0.7 / 3) * n_param
    init <- norm * init / sum(init)
  }
  return(init)
}

