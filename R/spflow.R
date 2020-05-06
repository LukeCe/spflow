spflow <- function(
  formula,
  sp_multi_network,
  origin_id,
  destination_id = origin_id,
  model_specifications
) {

  # check for abusive inputs
  assert(is(formula,"formula"),
         "A valid formula is required!")

  assert(is(sp_multi_network,"sp_multi_network"),
         "The data musst be a network data object!")

  assert(is(c(origin_id,destination_id),"character"),
         "The origin and destination id musst be strings!")

  assert(is(model_specifications,"character"),
         "If provided, the model specifications must be of specify_flow_model class!")

  # derive the interaction model data
  interaction_model_data <-

  od_pair_id <- origin_id %p% "_" %p% destination_id

  by_case_formula <- expand_flow_formula(formula)
  by_case_variables <- lapply(by_case_formula, get_all_var_names)


  # derive ihe interaction model frame




}
