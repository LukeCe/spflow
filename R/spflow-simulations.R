#' Simulate spatial interactions
#'
#' @param exogenous_variables
#'   A matrix of exogenous variables
#' @param model_coefficients
#'   A numeric vector of coefficients
#' @param inverted_filter
#'   A matrix that represents an inverted spatial filter matrix
#'   (see [spatial_filter()])
#' @param noise_sd
#'   A numeric which indicates the desired standard deviation of the simulated noise
#' @param verbose
#'   A logical whether signal to noise ration should be printed
#'
#' @family spflow simulation functions
#' @return A vector of simulated flows
#' @keywords internal
spflow_sim <- function(
  exogenous_variables,
  model_coefficients,
  inverted_filter,
  noise_sd,
  verbose = FALSE
) {

  # generate the flows
  signal <- inverted_filter %*% (exogenous_variables %*% model_coefficients)
  error <- rnorm(nrow(exogenous_variables),
                 sd = noise_sd)
  noise <- inverted_filter %*% error

  if (verbose) {
    cat("sd(noise)/sd(signal):\n")
    cat(sd(noise)/sd(signal), "\n")
  }

  return(as.vector(signal + noise))
}

#' @title Derive the flow neighborhood matrices
#'
#' @description
#' Use the neighborhood matrices of origins and destinations to derive the
#' three neighborhood matrices of the origin-destination flows.
#' This function is used for simulations or for comparisons with the vectorized
#' formulations of the model.
#'
#' @param OW Origin neighborhood matrix
#' @param DW Destination neighborhood matrix
#' @param n_o A numeric indicating the number of origins
#' @param n_d A numeric indicating the number of destinations
#' @param model A character indicating the model identifier
#'
#' @family spflow simulation functions
#' @importFrom Matrix Diagonal
#' @keywords internal
expand_flow_neighborhood <- function(
  OW,
  DW,
  n_o = OW %|!|% nrow(OW),
  n_d = DW %|!|% nrow(DW),
  model = "model_9") {

  # initialize weight matrices to NULL
  # then replace those that are required for the chosen model
  Wd <- NULL
  Wo <- NULL
  Ww <- NULL

  model_number <- as.integer(substr(model,7,7))

  d_models <- c(2,5:9)
  o_models <- c(3,5:9)
  w_models <- c(4,6,8,9)

  if (model_number %in% d_models) {
    assert(!is.null(n_o) && !is.null(DW),
           "Construction the destination weight matrix requires arguments" %p%
             " for n_o and DW!")
    Wd <- Diagonal(n_o) %x% DW
  }

  if (model_number %in% o_models) {
    assert(!is.null(n_o) && !is.null(DW),
           "Construction the origin weight matrix requires arguments" %p%
             " for n_d and OW!")
    Wo <- OW %x% Diagonal(n_d)

  }

  if (model_number %in% w_models) {
    assert(!is.null(n_o) && !is.null(DW),
           "Construction the origin weight matrix requires arguments" %p%
             " for DW and OW!")
    Ww <- OW %x% DW
  }

  return(compact(list("Wd" = Wd, "Wo" = Wo, "Ww" = Ww)))
}


#' @title Derive the spatial filter of an interaction model
#'
#' @description
#' Use the neighborhood matrices of origins and destinations to derive the
#' three neighborhood matrices of the origin-destination flows.
#'
#'
#' @param weight_matrices List of flow neighborhood matrices
#' @param autoreg_parameters A numeric containing values for the
#'     auto-regressive parameters
#'
#' @family spflow simulation functions
#' @importFrom Matrix Diagonal Matrix
#' @keywords internal
spatial_filter <- function(
  weight_matrices,
  autoreg_parameters) {

  combined_weight_matrices <-
    Map("*", safely_to_list(weight_matrices),autoreg_parameters)
  combined_weight_matrices <-
    Matrix(Reduce("+", combined_weight_matrices))

  N <- nrow(combined_weight_matrices)
  A <- Diagonal(N) - combined_weight_matrices
  return(A)
}


#' @title Derive the order of the spatial model
#'
#' @description
#' The order of a spatial model corresponds to the number of neighborhood
#' matrices that is used to describe the spatial dependence.
#'
#' @inheritParams spflow_control
#' @return An integer corresponding to the number of weight matrices
#' @keywords internal
spatial_model_order <- function(model = "model_9") {

  model_number <- substr(model,7,7)

  order0_models <- lookup(0,1)
  order1_models <- lookup(1,2:6)
  order2_models <- lookup(2,7)
  order3_models <- lookup(3,c(8,9))

  model_order <- c(order0_models,order1_models,order2_models,order3_models
  )[model_number]

  return(as.integer(model_order))
}
