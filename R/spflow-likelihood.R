partial_spflow_loglik <- function(rho,RSS,W_traces,n_o,n_d,model) {

  nb_rho <- length(rho)
  N <- n_o * n_d
  n <- n_o

  if (nb_rho == 1) {
    tau <- c(1, -rho[1])
    det_part <- lndetmc(rho[1], W_traces, n, model)
  }
  if (nb_rho == 2) {
    tau <- c(1, -rho[1], -rho[2])
    det_part <- fodet1(c(rho[1], rho[2], 0), W_traces, n)
  }
  if (nb_rho == 3) {
    tau <- c(1, -rho[1], -rho[2], -rho[3])
    if (model == "model_9") {
      det_part <- fodet1(rho, W_traces, n)
    } else {
      det_part <- lndetmc(rho[1], W_traces, n, "model_2") +
        lndetmc(rho[2], W_traces, n, "model_3")
    }
  }

  rss_part <- -N * log(tau %*% RSS %*% tau) / 2
  return(det_part + rss_part)
}
