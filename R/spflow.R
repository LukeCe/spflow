#' Spatial interaction model
#'
#' @param flow_formula A formulas corresponding to the structural interaction model
#' @param sp_multi_network A [sp_multi_network()] object.
#' @param network_pair_id A character indicating the id of a [sp_network_pair()]
#' @param flow_control A [spflow_control()] list to fine tune the estimation
#' @param use_sdm A logical which adds spatial lags of origin and destination attributes as explanatory variables to the model.
#' @param use_intra A logical which adds separate set of coefficients for intra-observational flows (origin == destination) to the model.
#'
#' @return A spflow_model object
#' @export
spflow <- function(
  flow_formula,
  sp_multi_network,
  network_pair_id = id(sp_multi_network)["network_pairs"][[1]][[1]][[1]],
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

  pair_exists <-
    any(network_pair_id == names(id(sp_multi_network)[["network_pairs"]]))
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
  model_moments <- spflow_model_moments(
    model_formulation,
    model_matrices,
    estimator = flow_control$estimation_method)

  # moment based estimation
  estimation_results <-
    spflow_model_estimation(model_moments,
                            estimator = flow_control$estimation_method)

  # add the data
  estimation_results$data <- drop_instruments(model_matrices)

  # solve the coefficient nameing
  rownames(estimation_results$results) <-
    parameter_names(model_matrices = estimation_results$data,
                    model_formulation = model_formulation,
                    model = flow_control$model)

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



parameter_names <- function(
  model_matrices,
  model_formulation,
  model) {

  names_rho <- identify_auto_regressive_parameters(model)

  if (model_formulation == "matrix") {

    names_const <- c("Constant", "Constant_intra")
    use_const <- c(model_matrices$const == 1,
                   !is.null(model_matrices$const_intra$In))
    names_const <- names_const[use_const]

    x_structures <- c("DX","OX","IX")
    names_X <- lapply(
      model_matrices[x_structures] %>% compact(),
      colnames) %>% unlist()

    names_G <- names(model_matrices$G)
    export_names <- c(names_rho,names_const,names_X,names_G)
  }

  if (model_formulation == "vector") {
    nb_flow_variables <- 1 + length(names_rho)
    export_names <- names(model_matrices)[-seq_len(nb_flow_variables)]
  }

  return(export_names)

}

drop_instruments <- function(model_matrices) {

  vector_treatment <- c("DX","OX","IX")
  instrument_positions <-
    lapply(model_matrices[vector_treatment] %>% compact(),
           attr, "is_instrument_var")
  matrices_1 <- mapply(
    FUN = "drop_matrix_columns",
    matrix = model_matrices[vector_treatment] %>% compact(),
    drop_index = instrument_positions %>% compact(),
    SIMPLIFY = FALSE)

  simple_treatment <- c("const","Y")
  matrices_2 <- lapply(model_matrices[simple_treatment] %>% compact(),
                       function(.l) .l[[1]])


  matrix_list_treatment <- c("const_intra","G")
  instrument_positions <-
    model_matrices[matrix_list_treatment] %>% compact() %>%
    lapply(. , function(.l) lapply(.l, attr, "is_instrument_var") %>% unlist())
  matrices_3 <- mapply(
    "drop_elements",
    object = model_matrices[matrix_list_treatment],
    drop_index = instrument_positions,
    SIMPLIFY = FALSE)

  nice_order <- c("Y","const","const_intra","DX","OX","IX","G")
  return(c(matrices_1,matrices_2,matrices_3)[nice_order])
}
