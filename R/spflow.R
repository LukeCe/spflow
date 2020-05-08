#' Spatial interaction model
#'
#' @inheritParams sp_network_pair
#' @param flow_forumula A formulas corresponding to the structural interaction model
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
  network_pair_id = ids(sp_multi_network,"network_pairs")[[1]],
  flow_control = spflow_control(),
  origin_network_id = NULL,
  destination_network_id = NULL,
  use_sdm = TRUE,
  use_intra = TRUE
) {

  ## check for abusive inputs ...
  assert(is(formula,"formula"),
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

  assert(is_single_logical(use_intra)
         && is_single_logical(use_sdm),
         "The arguments [use_intra and use_sdm] must be logicals of length 1!")

  # ... test the arguments provided by control by calling it again
  control <- do.call(spflow::control, control)

  flow_formulas <- expand_roles_and_cases(
    flow_formula,
    flow_control,
    use_intra)

  # The formulas for the explanatory variably have to extenden

  ## interpret the formulas to generate the model data

  od_pair_id <- origin_network_id %p% "_" %p% destination_network_id

  by_case_formula <- expand_main_formula(formula)
  by_case_variables <- lapply(by_case_formula, get_all_var_names)


  # derive ihe interaction model frame




}
