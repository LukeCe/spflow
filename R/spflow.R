#' Spatial interaction model
#'
#' @inheritParams sp_network_pair
#' @param flow_formula A formulas corresponding to the structural interaction model
#' @param sp_multi_network A [sp_multi_network()] object.
#' @param network_pair_id A character indicating the id of a [sp_network_pair()]
#' @param flow_control A [spflow_control()] list to fine tune the estimation
#' @param use_sdm A logical which adds spatial lags of origin and destination attributes as explanatory variables to the model.
#' @param use_intra A logical which adds sperate set of coefficients for intra-observational flows (origin == destination) to the model.
#'
#' @return A spflow_model object
#' @export
spflow <- function(
  flow_formula,
  sp_multi_network,
  network_pair_id =
    id(sp_multi_network)["network_pairs"][[1]]["network_pair_id"],
  flow_control = spflow_control()
) {

  ## check for abusive inputs ...
  assert(is(flow_formula,"formula"),
         "A valid formula is required!")

  assert(is(sp_multi_network,"sp_multi_network"),
         "The data musst be a network data object!")

  ## ... check correct types of ids
  assert(is_single_character(network_pair_id),
         "The network_pair_id musst be a character of length 1!")

  pair_exists <- any(network_pair_id == id(sp_multi_network)$network_pair_id)
  assert(pair_exists,
         "The the network pair id [%s] is not available!" %>%
           sprintf(., network_pair_id))

  ## ... test the arguments provided to control by calling it again
  flow_control <- do.call(spflow_control, flow_control)

  ## transform ...
  model_matrices <- spflow_model_matrix(
    sp_multi_network,
    network_pair_id,
    flow_formula,
    flow_control)

  # derive the model moments
  model_formulation <- "matrix"
  model_moments <- spflow_model_moments(model_formulation, model_matrices)

  # moment based estimation
  estimation_results <- spflow_model_estimation(model_moments,estimator)
  estimation_results$data <- model_matrices

  rownames(estimation_results$results) <-
    explanatory_varnames(model_matrices,model_formulation,model)

  calculate_residuals <- FALSE
  if (calculate_residuals) {
    stop("Not yet available")

    estimation_results$fitted_values <-
      predict(estimation_results)
    estimation_results$residuals <-
      model_data$Y - estimation_results$fitted_values

  }


  # return
  return(estimation_results)
}



explanatory_varnames <- function(
  model_matrices,
  model_formulation,
  model) {

  if (model_formulation == "matrix") {
    exogenous_variables <- c("const","const_intra","DX","OX","IX","G")
    explanatory_varnames <- rapply(model_matrices[exogenous_variables],
                                   colnames)
  }

  if (model_formulation == "vector") {
    nb_flow_variables <- 1 + (
      identify_auto_regressive_parameters(model) %>%
      length())
    explanatory_varnames <- names(model_matrices)[-seq_len(nb_flow_variables)]
  }

  return(explanatory_varnames)

}
