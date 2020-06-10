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
  flow_control = spflow_control(),
  origin_network_id = NULL,
  destination_network_id = NULL
) {

  ## check for abusive inputs ...
  assert(is(flow_formula,"formula"),
         "A valid formula is required!")

  assert(is(sp_multi_network,"sp_multi_network"),
         "The data musst be a network data object!")

  # ... check correct types of ids
  if (!is.null(origin_network_id) && !is.null(destination_network_id)) {

    assert(is_single_character(origin_network_id)
           && is_single_character(destination_network_id),
           "The origin and destination network ids musst be " %p%
           "characters of length 1!")

    network_pair_id <- concat_by("_",c(origin_network_id,
                                       destination_network_id))
  }

  assert(is_single_character(network_pair_id),
         "The network_pair_id musst be a character of length 1!")

  model_target <- id(sp_multi_network,network_pair_id = network_pair_id)
  assert(is.character(model_target),
         "The the network pair id [%s] is not available!" %>%
           sprintf(., network_pair_id))

  # ... test the arguments provided to control by calling it again
  flow_control <- do.call(spflow_control, flow_control)

  # transform ...
  model_matrices <- spflow_model_matrix(
    sp_multi_network,
    network_pair_id,
    flow_formula,
    flow_control)

  # derive the model moments
  model_moments <- spflow_model_moments()

  # moment based estimation
  estimation_results <- spflow_model_estimation(model_moments,estimator)

  estimation_results$data <- model_matrices
  estimation_results$fitted_values <- predict(estimation_results)
  estimation_results$residuals <- model_data$Y - estimation_results$fitted_values

  # return
  return(estimation_results)
}
