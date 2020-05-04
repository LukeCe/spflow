spflow <- function(
  formula,
  network_data,
  origin_id,
  destination_id = origin_id,
  model_specifications
) {

  # check for abusive inputs
  assert(is(formula,"formula"),
         "A valid formula is required!")

  assert(is(network_data,"network_data"),
         "The data musst be a network data object!")

  assert(is(c(origin_id,destination_id),"character"),
         "The origin and destination id musst be strings!")

  assert(is(model_specifications,"character"),
         "If provided, the model specifications must be of specify_flow_model class!")

  # check for inconsistent inputs



}
