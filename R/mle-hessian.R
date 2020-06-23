spflow_hessian <- function(hessian_method, ...) {

  args <- list(...)

  hessian_result <- switch(hessian_method,
    "mixed" = {
      spflow_mixed_hessian(numerical_hess = args$numerical_hess,
                           ZZ = args$ZZ,
                           ZY = args$ZY,
                           TSS = args$TSS,
                           N = args$N,
                           mu = args$mu,
                           sigma2 = args$sigma2)
      })

  return(hessian_result)
}

spflow_mixed_hessian <- function(
  numerical_hess,
  ZZ,
  ZY,
  TSS,
  N,
  mu,
  sigma2
) {


  # mu    = c(rho,delta)    = parameters of spatial interaction model
  # theta = c(delta,sigma2) = parameters of gaussian model
  size_rho <- ncol(numerical_hess)
  rho <- mu[seq_len(size_rho)]
  delta <- mu[-seq_len(size_rho)]

    # block 2,2 = theta,theta = standard linear model block
  hess_22 <- -Matrix::bdiag(ZZ/sigma2,N/(2*sigma2^2)) %>% as.matrix()

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
spflow_f2_hessian <- function(variables) {

}

spflow_exact_hessian <- function(variables) {

}
