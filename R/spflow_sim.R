invert_spatial_filter <- function(
  weight_matrices,
  autoreg_parameters
) {

  combined_weight_matrices <-
    weight_matrices %>%
    savely_to_list() %>%
    mapply("*", ., autoreg_parameters) %>%
    Reduce("+",.)

  N <- nrow(combined_weight_matrices)

  return(solve(diag(N) - combined_weight_matrices))
}

spflow_sim <- function(
  exogenous_variables,
  model_coeffiecients,
  inverted_filter,
  noise_sd,
  verbose = FALSE
) {

  # generate the flows
  signal <- inverted_filter %*% (exogenous_variables %*% model_coeffiecients)
  error <- rnorm(nrow(exogenous_variables),
                 sd = noise_sd)
  noise <- inverted_filter %*% error

  if (verbose) {
    cat("sd(noise)/sd(signal):\n")
    cat(sd(noise)/sd(signal), "\n")
  }

  return(as.vector(signal + noise))
}
