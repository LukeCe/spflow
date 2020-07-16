spflow_hessian <- function(hessian_method, hessian_inputs) {

  hessian_function <- "spflow_" %p% hessian_method %p% "_hessian"
  hessian_result <- do.call(hessian_function,hessian_inputs)

  return(hessian_result)
}

spflow_mixed_hessian <- function(
  numerical_hess,
  ZZ,
  ZY,
  TSS,
  N,
  rho,
  delta,
  sigma2
) {

  size_rho <- length(rho)

  # block 2,2 = theta,theta     = standard linear model block
  # ... theta = c(delta,sigma2) = parameters of gaussian model
  hess_22 <- -block_diag(ZZ/sigma2, N/(2*sigma2^2))

  # block 2,1 = interaction of rho and theta
  tau <- c(1, -rho)
  ZJ <- ZY[ ,-1, drop = FALSE]

  hess_21 <-
    rbind(-ZJ/sigma2, crossprod(delta,ZJ) - tau %*% TSS[, -1]) %>%
    matrix(ncol = size_rho)
  hess_12 <- t(hess_21)

  # block 1,1 = rho, rho = mixed numrical analytical solution
  hess_11 <- numerical_hess + hess_12 %*% solve(hess_22, hess_21)

  full_hessian <-
    rbind(cbind(hess_11, hess_12),
          cbind(hess_21, hess_22))

  return(full_hessian)
}

# TODO finish the hessian methods
spflow_f2_hessian <- function(
  ZZ,
  ZY,
  TSS,
  W_traces,
  n_o,
  n_d,
  rho,
  delta,
  delta_t,
  sigma2,
  model) {

  N <- n_o * n_d

  # from Lesage Matlab hessian function
  params <- c(rho,delta,sigma2)
  p <- length(params)

  index_rho <- seq_along(rho)
  index_delta <- seq_along(delta) + length(rho)
  index_sigma <- seq_along(sigma2) + length(c(rho,delta))


  # Compute the stepsize (h)
  eps <- .Machine$double.eps
  shift <- eps^(1 / 3) * abs(params)

  # new loglik value as a function of a shift in the parameter vector
  # IDEA use memiose for re_evaluate_logdet -> speed up calculation

  re_evaluate_loglik <- function(shift_vec){

    # shift input values
    shift_params <- params + shift_vec

    new_rho <- shift_params[index_rho]
    new_delta_t <- decompose_shift(
      delta_t = delta_t,
      rho     = new_rho,
      shift   = shift_vec[index_delta])

    # decomposed RSS and logdet
    new_RSS <- re_eval_RSS(new_delta_t,TSS,ZZ,ZY)
    det_value <- spflow_logdet(
      rho = rho,
      W_traces = W_traces,
      n_o = n_o,
      n_d = n_d,
      model = model)

    # complete RSS for the remainder of the loglik
    new_sigma2 <- shift_params[index_sigma]
    new_tau <- c(1, -new_rho)
    new_RSS_complete <- new_tau %*% new_RSS %*% new_tau
    ll_rest <- reminder_spflow_loglik(
      N = N, sigma2 = new_sigma2, RSS = new_RSS_complete)

    return(det_value + ll_rest)


  }

  # Compute "zero" step
  f0_loglik <- re_evaluate_loglik(numeric(p))

  # Compute "single" forward step
  shift_f1 <- diag(shift)
  f1_loglik <- numeric(p)
  for (i in 1:p) {
     f1_loglik[i] <- re_evaluate_loglik(shift_f1[ , i])
  }

  # Compute "double" forward step
  hess <- tcrossprod(shift)
  for (i in 1:p) {

    # diagonal elements
    f2_loglik <- re_evaluate_loglik(2 * shift_f1[, i])
    hess[i, i] <-
      (f2_loglik - 2 * f1_loglik[i]  + f0_loglik)/hess[i, i]

    for (j in seq_len(p - i)) {
      # exploit symmetry for off-diagonal elements
      c <- j + i
      f2_loglik <- re_evaluate_loglik(shift_f1[, i] + shift_f1[, c])

      hess[i, c] <- hess[c, i] <-
        (f2_loglik - f1_loglik[i] - f1_loglik[c]  + f0_loglik)/hess[i, c]
    }
  }

  return(hess)
}



spflow_exact_hessian <- function(variables) {

}
