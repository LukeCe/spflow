# combinatorics ---------------------------------------------------------------

#' @keywords internal
multinom_coef <- function(...) {

  k_args <- list(...) %>% flatlist()
  t <- Reduce("+",k_args)

  # calculate the denominator
  chose_k_factorial <- k_args %>%
    lapply(factorial) %>%
    Reduce(f = "*", .)

  return(factorial(t)/chose_k_factorial)
}

#' @keywords internal
count_trinom_coefs <- function(n) {
  (n + 1) * (n + 2) / 2
}

#' @keywords internal
sum_trinom_coefs <- function(n) {
  lapply(seq_len(n), count_trinom_coefs) %>%
    Reduce("+",.) %>%
    as.integer()
}
