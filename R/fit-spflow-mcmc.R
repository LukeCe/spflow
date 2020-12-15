#' Moment based estimation procedure for the MCMC estimator
#'
#' @keywords internal
spflow_mcmc <- function(
  ZZ,
  ZY,
  TSS,
  N,
  n_d,
  n_o,
  OW_traces,
  DW_traces,
  flow_control = flow_control,
  nb_draw = 5500,
  nb_burn_in = 2500
) {

  model <- flow_control$model
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

  collect_delta  <- matrix(0, nrow = nb_draw + 1, ncol = size_delta)
  collect_sigma2 <- matrix(0, nrow = nb_draw + 1, ncol = 1)
  collect_rho    <- matrix(0, nrow = nb_draw + 1, ncol = nb_rho)

  # the first draws are equal to uninformative prior distributions
  collect_delta[1,] <- 0
  collect_sigma2[1,] <- 0
  collect_rho[1,] <- pre_rho
  shape_sigma2 <- N/2


  ## for adaptive M-H sampling we need to monitor the acceptance rate
  # the sample uses a tuned random walk procedure initialized at 0.2
  acceptance_rate <- rep(0,nb_rho)
  tune_rw <- rep(0.2,nb_rho)

  # pre-compute quantities that are used repeatedly
  varcov_delta <- chol2inv(chol(ZZ))
  varcov_delta_chol <- chol(varcov_delta)
  fast_multi_rnorm <- function(n, sd = NULL) {
    rnorm(n,sd = sd) %*% varcov_delta_chol
  }
  delta_t <- solve(ZZ,ZY)


  # we also calculate an initial log-determinant value ...
  #... and pass it to the mcmc variable
  previous_log_det <- spflow_logdet(pre_rho,OW_traces, n_o, n_d,model)

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
    RSS <- update_RSS(TSS,ZZ,ZY,delta_updated,tau)
    sigma2_updated <- 1/rgamma(1, shape = shape_sigma2, rate = RSS / 2)
    collect_sigma2[i_mcmc  + 1] <- sigma2_updated


    ## 3. Metropolis Hastings sampling for rho..

    # 3.1) Generation of candidates
    # ... sample jointly candidates for new values of rho that respect:
    # ... the interval [-LB, UB] for each rho
    # ... the stability restriction |sum(rho)| < SR
    # ... the stability restriction sum(|rho|) < SR
    # ... non-zero log-determinant (-> A non-singular): not applied
    valid_candidates <- FALSE
    count_draws <- 1
    maximal_draw <- 100
    while (!valid_candidates & count_draws < maximal_draw) {

      candidate_rho <- collect_rho[i_mcmc ,] + rnorm(nb_rho) * tune_rw

      valid_candidates <- (
        min(candidate_rho) > bound_rho["low"]
        & max(candidate_rho) < bound_rho["up"]
        & abs(sum(candidate_rho)) < bound_sum_rho
        & sum(abs(candidate_rho)) < bound_sum_abs_rho
      )
      count_draws <- count_draws + 1
      count_failed_candidates <-
        count_failed_candidates + (count_draws >= maximal_draw)
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

      # # draw candidates that lead to non-singular filter matrices
      # count_candidates <- 1
      # while (!valid_c[j]) {
      #   # Sampled candidates have to fulfil four constraints
      #   # ... the interval [-LB, UB] for each rho
      #   # ... the stability restriction |sum(rho)| < SR
      #   # ... the log-determinant must not be zero (-> A non-singular)
      #   rho_c1 <- previous_rho[j] + rnorm(1) * tune_rw[j]
      #   updated_rho[j] <- rho_c1
      #   valid_c[j] <- ( # check linear constraints
      #     rho_c1 > bound_rho["low"]
      #     & rho_c1 < bound_rho["up"]
      #     & abs(sum(updated_rho)) < bound_sum_rho
      #     & sum(abs(updated_rho)) < bound_sum_abs_rho
      #   )
      #
      #   if (valid_c[j]) {
      #     updated_log_det <- spflow_logdet(updated_rho,OW_traces,n_o, n_d, model)
      #     # true constraint base on log-determinant value is not yet stable
      #     valid_c[j] <- log_det_minimum < updated_log_det
      #   }
      #
      #   count_candidates <- 1 + count_candidates
      #   if (count_candidates >= 100){
      #     stop("Can not generate valid candidates for the auto-regressive" %p%
      #            " parameters")
      #   }
      # }
      updated_rho[j] <- candidate_rho[j]
      tau_c <- c(1, -updated_rho)
      delta_c <- as.vector(delta_t %*% tau_c) + deviate_delta

      # updated RSS and log-determinant
      RSS_c <- update_RSS(TSS,ZZ,ZY,delta_c,tau_c)
      RSS_diff <- (RSS_c - RSS) / (2*sigma2_updated)
      updated_log_det <- spflow_logdet(updated_rho,OW_traces,n_o, n_d, model)
      proba_ratio <- exp(updated_log_det - previous_log_det - RSS_diff)

      accept[j] <- (proba_ratio > accept_hurdle[j])
      if (!accept[j]) { # reject: -> remain and previous
        updated_rho[j] <- previous_rho[j]
      }
      if (accept[j]) { # accept: -> updated values are used for next iterations
        previous_log_det <- updated_log_det
        RSS <- RSS_c
      }

    }
    collect_rho[i_mcmc + 1, ] <- updated_rho


    # monitor acceptance rate to tune the random walk procedure
    acceptance_rate <- ((acceptance_rate * (i_mcmc - 1) + accept * 1) / i_mcmc)
    abnorm_rate <- abs(acceptance_rate - 0.5) > 0.1
    adjust_ratio <- 1.1 ^ (sign(acceptance_rate - 0.5))
    tune_rw[abnorm_rate] <- tune_rw[abnorm_rate] * adjust_ratio[abnorm_rate]

  }


  mcmc_results <- cbind(collect_rho[-(1:nb_burn_in),],
                        collect_delta[-(1:nb_burn_in),],
                        collect_sigma2[-(1:nb_burn_in),])

  results_df <- data.frame(
    est = colMeans(mcmc_results),
    sd = apply(mcmc_results, 2, sd),
    lower_05 = apply(mcmc_results, 2, quantile, 0.05),
    lower_95 = apply(mcmc_results, 2, quantile, 0.95)
  )

  N <- n_o * n_d
  results_df$"t.stat" <- results_df$est / results_df$sd
  results_df$"p.value" <- 1 - pt(q = abs(results_df$est / results_df$sd),
                                 df =  1)

  id_sd <- nrow(results_df)
  estimation_results <- spflow_model(
    mcmc_results = mcmc_results,
    estimation_results = results_df[-id_sd, ],
    estimation_control = flow_control,
    sd_error = sqrt(results_df$est[id_sd]),
    N = N
  )

  return(estimation_results)
}
