# combinatorics ---------------------------------------------------------------

#' @keywords internal
multinom_coef <- function(...) {

  k_args <- flatlist(list(...))
  t <- Reduce("+",k_args)

  # calculate the denominator
  chose_k_factorial <- Reduce("*", lapply(k_args , factorial))
  return(factorial(t)/chose_k_factorial)
}

#' @keywords internal
count_trinom_coefs <- function(n) {
  (n + 1) * (n + 2) / 2
}

#' @keywords internal
sum_trinom_coefs <- function(n) {
  as.integer(Reduce("+",lapply(seq_len(n), count_trinom_coefs)))
}
