#' @keywords internal
spflow_hessian <- function(hessian_method, hessian_inputs) {

  hessian_function <- "spflow_" %p% hessian_method %p% "_hessian"
  hessian_result <- do.call(hessian_function,hessian_inputs)

  return(hessian_result)
}

#' @keywords internal
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
  sigma4 <- sigma2^2
  hess_22 <- -block_diag(ZZ/sigma2, N/(2*sigma4))

  # block 2,1 = interaction of rho and theta
  tau <- c(1, -rho)
  ZJ <- ZY[ ,-1, drop = FALSE]

  hess_21 <- rbind(-ZJ/sigma2,
                   (crossprod(delta,ZJ) - tau %*% TSS[, -1])/sigma4)
  hess_21 <- matrix(hess_21, ncol = size_rho)
  hess_12 <- t(hess_21)

  # block 1,1 = rho, rho = mixed numerical analytical solution
  hess_11 <- numerical_hess + hess_12 %*% solve(hess_22, hess_21)

  full_hessian <-
    rbind(cbind(hess_11, hess_12),
          cbind(hess_21, hess_22))

  return(full_hessian)
}

#' @keywords internal
spflow_f2_hessian <- function(
  ZZ,
  ZY,
  TSS,
  N,
  rho,
  delta,
  delta_t,
  sigma2,
  calc_log_det) {

  # TODO update f2 Hessian
  params <- c(rho,delta,sigma2)
  p <- length(params)

  index_rho <- seq_along(rho)
  index_delta <- seq_along(delta) + length(rho)
  index_sigma <- seq_along(sigma2) + length(c(rho,delta))


  # Compute the step-size (h)
  eps <- .Machine$double.eps
  shift <- sqrt(eps) * abs(params)

  # IDEA spare some log determinant and RSS evaluations by splitting
  # .... the parameters delta,rho,sigma
  re_evaluate_loglik <- function(shift_vec){

    # shift input values
    shift_params <- params + shift_vec
    new_rho <- shift_params[index_rho]
    new_sigma2 <- shift_params[index_sigma]
    new_delta <- shift_params[index_delta]

    # evaluate the RSS and log-determinant and the rest of the LL
    new_tau <- c(1, -new_rho)
    new_RSS <- update_RSS(TSS,ZZ,ZY,new_delta,new_tau)
    det_value <- calc_log_det(new_rho)
    ll_rest <- reminder_spflow_loglik(N, new_sigma2, new_RSS)

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

#' @keywords internal
reminder_spflow_loglik <- function(N,sigma2,RSS){

  ll_without_logdet <-
    -((N / 2) * log(pi)) - ((N / 2) * log(sigma2)) - (RSS / (2 * sigma2))

  return(ll_without_logdet)
}
