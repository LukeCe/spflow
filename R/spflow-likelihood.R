# function for likelihood based estimation: MLE and MCMC
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

spflow_logdet <- function(rho,W_traces,n_o,n_d,model){

  nb_rho <- length(rho)
  N <- n_o * n_d
  n <- n_o

  if (nb_rho == 1) {
    tau <- c(1, -rho[1])
    log_det <- lndetmc(rho[1], W_traces, n, model)
  }
  if (nb_rho == 2) {
    tau <- c(1, -rho[1], -rho[2])
    log_det <- fodet1(c(rho[1], rho[2], 0), W_traces, n)
  }
  if (nb_rho == 3) {
    tau <- c(1, -rho[1], -rho[2], -rho[3])
    if (model == "model_9") {
      log_det <- fodet1(rho, W_traces, n)
    } else {
      log_det <- lndetmc(rho[1], W_traces, n, "model_2") +
        lndetmc(rho[2], W_traces, n, "model_3")
    }
  }

  return(log_det)
}

reminder_spflow_loglik <- function(N,sigma2,RSS){

  ll_without_logdet <-
    -((N / 2) * log(pi)) - ((N / 2) * log(sigma2)) - (RSS / (2 * sigma2))

  return(ll_without_logdet)
}

decompose_shift <- function(delta_t,rho,shift){

  tau <- c(1, -rho)
  scaled_shift <- shift/sum(tau)

  delta_t <- delta_t + scaled_shift
  return(delta_t)
}

re_eval_RSS <- function(delta_t,TSS,ZZ,ZY){

  ESS_tz <- crossprod(delta_t,ZZ) %*% delta_t
  ESS_tzy <- crossprod(delta_t, ZY)
  RSS <- TSS - (2 * ESS_tzy) + ESS_tz
  return(RSS)

}

draw_initial_guess <- function(n_param) {
  init <- runif(n_param)

  if (n_param > 1) {
    norm <- (0.7 / 3) * n_param
    init <- norm * init / sum(init)
  }
  return(init)
}
