spflow <- function(
  formula,
  sp_multi_network,
  network_pair_id = ids(sp_multi_network,"network_pairs")[[1]],
  control = spflow::control(),
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

  ### Interpret the flow formula
  # 1. The flow formula must be carried to all data sources...
  #    [Interactions] ~ [Origins] + [Destinations] + [Intra] + [Pair]
  # 2. The formula must account for lagged (SDM) variables and instruments
  flow_formulas <- expand_flow_formula(formula)

  # The formulas for the explanatory variably have to extenden

  ## interpret the formulas to generate the model data

  od_pair_id <- origin_network_id %p% "_" %p% destination_network_id

  by_case_formula <- expand_flow_formula(formula)
  by_case_variables <- lapply(by_case_formula, get_all_var_names)


  # derive ihe interaction model frame




}
