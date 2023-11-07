#' Simulate spatial interactions
#'
#' @param exogenous_variables
#'   A matrix of exogenous variables
#' @param model_coefficients
#'   A numeric vector of coefficients
#' @param filter_matrix
#'   A matrix that represents the [spatial_filter()] of an interaction model
#' @param noise_sd
#'   A numeric which indicates the desired standard deviation of the simulated noise
#' @param verbose
#'   A logical whether signal to noise ration should be printed
#'
#' @family spflow simulation functions
#' @return A vector of simulated flows
#' @noRd
#' @keywords internal
spflow_sim <- function(
  exogenous_variables,
  model_coefficients,
  filter_matrix,
  noise_sd,
  verbose = FALSE
) {

  # generate the flows
  signal <- exogenous_variables %*% model_coefficients
  error <- rnorm(nrow(exogenous_variables), sd = noise_sd)

  filtered_input <- solve(filter_matrix, cbind(signal,error))

  if (verbose) {
    cat("sd(noise)/sd(signal):\n")
    cat(sd(filtered_input[,2])/sd(filtered_input[,1]), "\n")
  }

  return(rowSums(filtered_input))
}
