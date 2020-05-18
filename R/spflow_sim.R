#' Create an invertet spatial filter that can be used for simulations
#'
#' @param weight_matrices A list of neighborhood matrices
#' @param autoreg_parameters A vector of parameters
#'
#' @return
#' @export
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

#' Simulate spatial interactions
#'
#' @param exogenous_variables A matrix of exogenouse variables
#' @param model_coeffiecients A numeric vector of coefficients
#' @param inverted_filter A matrix that represents an inverted spatial filter matrix (see [invert_spatial_filter()])
#' @param noise_sd A numeric which indicates the desired standard deviation of the simulated noise
#' @param verbose A logical whether signal to noise ration should be printed
#'
#' @return A vector of simulated flows
#' @export
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
