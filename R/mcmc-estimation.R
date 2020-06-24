spflow_mcmc <- function(
  ZZ,
  ZY,
  TSS,
  N,
  n_d,
  n_o,
  OW_traces,
  DW_traces,
  model,
  nb_draw = 5500,
  nb_burn_in = 2500
) {


  ## intialize rho for M-H sampling
  nb_rho <- ncol(TSS) - 1
  pre_rho <- draw_inital_guess(nb_rho)
  updated_rho <- pre_rho
  bound_rho <- c("low" = -1, "up" = 1)

  ## create collectors for each parameter
  size_delta <- ncol(ZZ)

  collect_delta  <- matrix(nrow = nb_draw + 1, ncol = size_delta)
  collect_sigma2 <- matrix(nrow = nb_draw + 1, ncol = 1)
  collect_rho    <- matrix(nrow = nb_draw + 1, ncol = nb_rho)

  # the first draws are equal to uninformative priors
  collect_delta[1,] <- 0
  collect_sigma2[1,] <- 0
  collect_rho[1,] <- pre_rho
  shape_sigma2 <- N/2

  ## for adaptative M-H sampling we need to monitor the acceptance rate
  # the sample uses a tuned random walk procedure intialized at 0.2
  acceptance_rate <- rep(0,nb_rho)
  tune_rw <- rep(0.2,nb_rho)

  # precompute quantities that are used repeatedly
  varcov_delta <- chol2inv(chol(ZZ))
  varcov_delta_chol <- chol(varcov_delta)
  fast_multi_rnorm <- function(n, sd = NULL) {
    rnorm(n,sd = sd) %*% varcov_delta_chol
  }
  delta_decomposed <- solve(ZZ,ZY)


  ## begin MCMC sampling ----
  for (i_mcmc in 1:nb_draw) {

    ## 1. multivariate normal for beta ##

    # update mean with previous draw of rho
    tau <- c(1 , -collect_rho[i_mcmc,])
    mean_delta <- as.vector(tcrossprod(varcov_delta,tcrossprod(tau,ZY)))

    # update the sd with previous draw from sigma2
    sd_delta <- sqrt(collect_sigma2[[i_mcmc]])

    # construct updated delta using a random deviation
    deviate_delta <-
      fast_multi_rnorm(n = size_delta, sd = sd_delta) %>%
      as.vector()

    delta_updated <- mean_delta + deviate_delta
    collect_delta[i_mcmc + 1,] <- delta_updated

    ## 2. inverse gamma for sigma2 ##
    # split the deviate equally on the decomposed delta keeping
    # the variance of the sum constant
    delta_t <- delta_decomposed + deviate_delta / sum(tau^2)

    # construct resudial based on previous values of rho and delta
    # update scale parameter besed on the new RSS

    # update scale parameter besed on the new RSS
    ESS_tz <- crossprod(delta_t,ZZ) %*% delta_t
    ESS_tzy <- crossprod(delta_t, ZY)
    RSS <- TSS - 2 * ESS_tzy + ESS_tz

    # collapse the decoposed RSS into the single one
    mcmc_step2_RSS <- tau %*% RSS %*% tau

    collect_sigma2[i_mcmc  + 1] <-
      1/rgamma(1,shape = shape_sigma2, rate = mcmc_step2_RSS / 2)


    ## 3. Metropolis Hastings for rho
    # ... sample jointly candidates for new values of rho that respect:
    # ... the interval [-1 , 1] for each rho
    # ... the stability restriction |sum(rho)| < 1
    instable_rho <- TRUE
    count_draws <- 1
    maximal_draw <- 100
    while (instable_rho & count_draws < maximal_draw) {

      candidate_rho <- collect_rho[i_mcmc ,] + rnorm(nb_rho) * tune_rw

      instable_rho <- (
        min(candidate_rho) < bound_rho["low"]
        || max(candidate_rho) > bound_rho["up"]
        || abs(sum(candidate_rho)) > 1
      )
      count_draws <- count_draws + 1
    }

    # for each rho, acceptance of the new candidate depends on the ratio of the
    # conditional likelihoods for updated and previous values
    # when the new candidate has higher value we accept it
    # when the new candidate has lower value we accept with probability p
    # ... where p is the ratio of likelihoods
    accept_hurdle <- runif(nb_rho)
    previous_rho <- collect_rho[i_mcmc ,]
    accept <- vector("logical", nb_rho)

    # # FIXME remve debug soltion
    # debug_rho_solutio <- c(0.45, 0.32, -0.21)
    # updated_rho <- debug_rho_solutio
    # collect_rho[i_mcmc + 1, ] <- updated_rho
    # accept <- !accept

    for (j in seq_along(candidate_rho)) {
      updated_rho[j] <- candidate_rho[j]

      proba_previous <- partial_spflow_loglik(
        rho = previous_rho,
        RSS = RSS,W_traces = OW_traces,model = model, n_o = n_o, n_d = n_d)

      proba_updated <- partial_spflow_loglik(
        rho = updated_rho,
        RSS = RSS,W_traces = OW_traces,model = model, n_o = n_o, n_d = n_d)
      proba_ratio <- exp(proba_updated - proba_previous)

      accept[j] <- (proba_ratio > accept_hurdle[j])
      if (!accept[j]) {
        updated_rho[j] <- previous_rho[j]
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
                                 df =  N - length(delta))

  id_sd <- nrow(results_df)
  estimation_results <- spflow_model(
    results_df = results_df[-id_sd, ],
    varcov = NULL,
    sd_error = sqrt(results_df$est[id_sd]),
    N = N,
    method = "mcmc",
    mcmc_results = mcmc_results
  )

  return(estimation_results)
}


