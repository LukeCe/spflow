#' @importFrom coda as.mcmc
#' @keywords internal
spflow_mcmc <- function(
  ZZ,
  ZY,
  TSS,
  N,
  n_d,
  n_o,
  TCORR,
  estimation_control,
  pspace_validator,
  logdet_calculator) {

  model <- estimation_control$model
  nb_draw <- estimation_control$mcmc_iterations
  nb_burn_in <- estimation_control$mcmc_burn_in
  resampling_limit <- estimation_control$mcmc_resampling_limit

  # pre-compute quantities that are used repeatedly
  delta_x <- delta_t <- qr.coef(qr(ZZ), ZY)
  dd <- !is.na(delta_t[,1])
  assert(all(dd) | estimation_control[["allow_singular"]],
         "Encountered singular fit!")

  delta_t <- delta_t[dd,,drop=FALSE]
  ZY <- ZY[dd,,drop=FALSE]
  ZZ <- ZZ[dd,dd,drop=FALSE]
  RSS_t <- TSS - crossprod(ZY,delta_t)
  varcov_delta <- chol2inv(chol(ZZ))
  varcov_delta_chol <- chol(varcov_delta)
  fast_multi_rnorm <- function(n, sd = NULL) rnorm(n,sd = sd) %*% varcov_delta_chol


  ## initialize rho for M-H sampling
  nb_rho <- ncol(ZY) - 1
  pre_rho <- draw_initial_guess(nb_rho)
  bound_rho <- c("low" = -1, "up" = 1)
  bound_sum_rho <- 1
  bound_sum_abs_rho <- 2
  log_det_minimum <- log(sqrt(.Machine$double.eps))
  count_failed_candidates <- 0

  ## create collectors for each parameter
  size_delta <- ncol(ZZ)
  collect_delta  <- matrix(
    0, nrow = nb_draw + 1, ncol = size_delta,
    dimnames = list(NULL, colnames(ZZ)))
  collect_sigma2 <- matrix(
    0, nrow = nb_draw + 1, ncol = 1,
    dimnames = list(NULL, "sigma2"))
  collect_rho <- matrix(
    0, nrow = nb_draw + 1, ncol = nb_rho,
    dimnames = list(NULL, define_spatial_lag_params(model)))

  # the first draws are equal to uninformative prior distributions
  collect_delta[1,] <- 0
  collect_sigma2[1,] <- 0
  collect_rho[1,] <- pre_rho
  shape_sigma2 <- N/2

  ## for adaptive M-H sampling we need to monitor the acceptance rate
  # the sample uses a tuned random walk procedure initialized at 0.2
  # we also calculate an initial log-determinant value ...
  acceptance_rate <- rep(0,nb_rho)
  tune_rw <- rep(0.2,nb_rho)
  previous_log_det <- logdet_calculator(pre_rho)

  ## begin MCMC sampling ----
  for (i_mcmc in 1:nb_draw) {

    ## 1. multivariate normal for beta ##

    # random deviate
    deviate_delta <-
      as.vector(fast_multi_rnorm(size_delta, sqrt(collect_sigma2[i_mcmc])))

    # add the deviation to the mean vector
    tau <- c(1 , -collect_rho[i_mcmc,])
    delta_updated <- as.vector(delta_t %*% tau) + deviate_delta
    collect_delta[i_mcmc  + 1,] <- delta_updated

    ## 2. inverse gamma for sigma2 ##
    # to update scale parameter based on the new RSS
    # construct residuals based on previous values of rho and delta
    RSS_mean <- tau %*% RSS_t %*% tau
    RSS_deviate <- deviate_delta %*% ZZ %*% deviate_delta
    sigma2_updated <- 1/rgamma(1, shape = shape_sigma2,
                               rate = (RSS_mean + RSS_deviate) / 2)
    collect_sigma2[i_mcmc  + 1] <- sigma2_updated


    ## 3. Metropolis Hastings sampling for rho..

    # 3.1) Generation of candidates that satisfy the parameter space
    valid_candidates <- FALSE
    count_draws <- 1
    while (!valid_candidates & count_draws <= resampling_limit) {

      candidate_rho <- collect_rho[i_mcmc ,] + rnorm(nb_rho) * tune_rw
      valid_candidates <- pspace_validator(candidate_rho)
      count_draws <- count_draws + 1
      count_failed_candidates <-
        count_failed_candidates + (count_draws >= resampling_limit)
    }

    # 3.2) Metropolis Hastings step
    # for each rho, acceptance of the new candidate depends on the ratio of the
    # conditional likelihoods for updated and previous values
    # when the new candidate has higher value we accept it
    # when the new candidate has lower value we accept with probability p
    # ... where p is the ratio of likelihoods
    accept_hurdle <- runif(nb_rho)
    updated_rho <- previous_rho <- collect_rho[i_mcmc ,]
    accept <- vector("logical", nb_rho)

    for (j in seq_along(previous_rho)) {

      updated_rho[j] <- candidate_rho[j]
      tau_candidate <- c(1, -updated_rho)

      # updated RSS and log-determinant
      RSS_candidate <- tau_candidate %*% RSS_t %*% tau_candidate
      RSS_diff <- (RSS_candidate - RSS_mean) / (2*sigma2_updated)
      candidate_log_det <- logdet_calculator(updated_rho)
      proba_ratio <- exp(candidate_log_det - previous_log_det - RSS_diff)

      accept[j] <- (proba_ratio > accept_hurdle[j])
      if (!accept[j]) { # reject: -> remain at previous
        updated_rho[j] <- previous_rho[j]
      }
      if (accept[j]) { # accept: -> updated values are used for next iterations
        previous_log_det <- candidate_log_det
        RSS_mean <- RSS_candidate
      }

    }
    collect_rho[i_mcmc + 1, ] <- updated_rho


    # monitor acceptance rate to tune the random walk procedure
    acceptance_rate <- ((acceptance_rate * (i_mcmc - 1) + accept * 1) / i_mcmc)
    abnorm_rate <- abs(acceptance_rate - 0.5) > 0.1
    adjust_ratio <- 1.1 ^ (sign(acceptance_rate - 0.5))
    tune_rw[abnorm_rate] <- tune_rw[abnorm_rate] * adjust_ratio[abnorm_rate]

  }

  mcmc_results <- cbind(collect_rho,collect_delta,collect_sigma2)
  mcmc_results2 <- mcmc_results[-seq_len(nb_burn_in), -ncol(mcmc_results)]
  results_df <- create_results(
    est = colMeans(mcmc_results2),
    quant_025 = apply(mcmc_results2, 2, quantile, 0.025),
    quant_975 = apply(mcmc_results2, 2, quantile, 0.975),
    sd = apply(mcmc_results2, 2, sd),
    df = N - ncol(mcmc_results))
  results_df2 <- data.frame(row.names = c(colnames(collect_rho),rownames(delta_x)))
  results_df2[row.names(results_df),colnames(results_df)] <- results_df

  rho <- results_df$est[seq_len(ncol(collect_rho))]
  estimation_diagnostics <- list(
    "sd_error" = sqrt(mean(collect_sigma2[-seq_len(nb_burn_in)])),
    "varcov" = cor(mcmc_results[-seq_len(nb_burn_in),, drop = FALSE]),
    "model_coherence" = ifelse(pspace_validator(rho), "Validated", "Unknown"),
    "mcmc_results" = as.mcmc(mcmc_results))
  if (isTRUE(estimation_control[["track_condition_numbers"]]))
    estimation_diagnostics <- c(estimation_diagnostics, "rcond" = rcond(ZZ))


  estimation_results <- spflow_model(
    estimation_results = results_df2,
    estimation_control = estimation_control,
    estimation_diagnostics = estimation_diagnostics)

  return(estimation_results)
}

