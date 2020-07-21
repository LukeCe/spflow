#' Estimate spatial interaction models that incorporate spatial dependence
#'
#' @description
#' We implement three diffrent estimators of spatial interaction models that
#' allow to estimate origin-destination flows with spatial auto-correlation.
#'
#' The function allows to include spatial lags of the explanatory, which will
#' corresponds to a so called Spatial Durbin model (SDM)
#' \insertCite{Anselin1988;textual}{spflow}.
#' Morever, we give the option to include an additional set of coefficients
#' for intra-regional flows (origin = destination), as proposed by
#' \insertCite{LeSage2009;textual}{spflow}.
#' Both options are activated by default and can be adjusted through the
#' [spflow_control()] object.
#'
#' @details
#' Our estimation procedures make use of the matrix formulation introduced by
#' \insertCite{LeSage2008;textual}{spflow} and further developed by
#' (#Dargel2020Improve) to reduce the computational effort and memory
#' requirements considerbly.
#' See (#Dargel2020Sim) for an exhaustive simulation study.
#' The estimation procedure can be adjusted through the estimation_method
#' in [spflow_control()].
#'
#' @section Maximum likelihood estimation (MLE):
#' Maximum likelihood estimation is the default estimation procedure.
#' The estimatoion was first developed by
#' \insertCite{LeSage2008;textual}{spflow} and then improved by
#' (#Dargel2020Improve).
#'
#' @section Spatial two-stage least squares (S2SLS):
#' The S2SLS estimator if an adaption of the one proposed by
#' \insertCite{Kelejian1998;textual}{spflow}, to the case origin-destination
#' flows, with a double spatial index and up to three neighborhood matrices
#' (#Dargel2020Improve).
#' A similar estimation is done by \insertCite{tamesueDealingIntraregionalFlows2016;textual}{flowR}.
#'
#' @section Bayesian Markov Chain Monte Carlo (MCMC):
#' The MCMC estimator is based on the ideas of
#' \insertCite{LeSage2009;textual}{spflow} and incorporates the improvements
#' proposed in (#Dargel2020Improve).
#' Estimation is based on a tuned Metropolis-Hastings sampler for the
#' auto-regressive parameters, and for the remaining parameters it uses Gibbs
#' sampling.
#' The routine uses 5500 iterations of the sampling procedure and considers the
#' first 2500 as burn-in period.
#'
#' @section Formula interface:
#' The function offers a formula interface adapted to spatial interaction
#' models, which has the follwing structure:
#' `Y ~ O_(X1) + D_(X2) + I_(X3) + G_(X4)`
#' This structre refelcts the diffrent datasources involved in such a model.
#' On the left hand side is the independent variable `Y` which corresponds to
#' the vector of flows.
#' On the right hand side we have all the explanatory variables.
#' The functions `O_(...)` and `D_(...)` indicate which variables are used as
#' characteristics of the origins and destinations respectively.
#' Similarly, `I_(...)` indicates variables that should be used for the
#' intra-regional coefficients.
#' Finllay `G_(...)` declares which variables describe origin-destination
#' pairs, which most frequently will be a measure of distance.
#'
#' All the declared variables must be available in the provided
#' [sp_multi_network()], which gathers information on the origins and
#' destinations [sp_network()] as well as the origin-destination pairs
#' [sp_network_pair()].
#'
#' Using the shortcut notation `Y ~ .` is possible and will be interpreted as
#' usually, in the sense that we use all variables that are available for each
#' data source.
#' Also formulas such as `Y ~ . + G_(log(X4) + 1)` are possible. When the dot
#' shortcut is combined with explicit declaration it will only be used for the
#' non declared data sources.
#' The previous example will hence extend to
#' `Y ~ O_(.) + D_(.) + I_(.) + G_(log(X4) + 1)`.W
#'
#' @references \insertAllCited{}
#'
#' @param flow_formula A formulas corresponding to the structural interaction model
#' @param sp_multi_network A [sp_multi_network()] object.
#' @param network_pair_id A character indicating the id of a [sp_network_pair()]
#' @param flow_control A [spflow_control()] list to fine tune the estimation
#'
#' @family network_info
#' @seealso flow_control
#'
#' @return A spflow_model object
#' @export
spflow <- function(
  flow_formula,
  sp_multi_network,
  network_pair_id = id(sp_multi_network)["network_pairs"][[1]][[1]][[1]],
  flow_control = spflow_control()
) {

  ## ... check for abusive inputs
  assert(is(flow_formula,"formula"),
         "A valid formula is required!")

  assert(is(sp_multi_network,"sp_multi_network"),
         "The data musst be a network data object!")

  ## ... check correct types of ids
  assert(is_single_character(network_pair_id),
         "The network_pair_id musst be a character of length 1!")

  network_ids <- id(sp_multi_network)[["network_pairs"]][[network_pair_id]]
  assert(!is.null(network_ids),
         "The the network pair id [%s] is not available!" %>%
           sprintf(., network_pair_id))

  ## ... identify the flow type
  flow_control$flow_type <- ifelse(
    network_ids["origin_network_id"] ==
      network_ids["destination_network_id"],
    yes = "within", no = "between"
  )

  ## ... test the arguments provided to control by calling it again
  flow_control <- do.call(spflow_control, flow_control)

  ## ... create the design matrix/matrices
  model_matrices <- spflow_model_matrix(
    sp_multi_network,
    network_pair_id,
    flow_formula,
    flow_control)

  ## ... derive the model moments
  model_moments <- spflow_model_moments(
    formulation =  flow_control$formulation,
    model_matrices,
    estimator = flow_control$estimation_method,
    flow_type = flow_control$flow_type)

  # ... fit the model and add complementary information to the results
  estimation_results <- spflow_model_estimation(model_moments,flow_control)

  estimation_results$"data" <- drop_instruments(model_matrices)
  estimation_results$"formulation" <- flow_control$formulation
  estimation_results$"model" <- flow_control$model
  estimation_results$"auto-corr" <- ifelse(flow_control$use_sdm,"SDM","LAG")

  rownames(estimation_results$results) <-
    parameter_names(model_matrices = estimation_results$data,
                    model_formulation = estimation_results$formulation,
                    model = estimation_results$model)

  # TODO solve the residual and fitted value issue
  calculate_residuals <- FALSE
  if (calculate_residuals) {
    stop("Not yet available")

    estimation_results$fitted_values <-
      predict(estimation_results)
    estimation_results$residuals <-
      estimation_results$data$Y - estimation_results$fitted_values
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
    lapply(function(.l) lapply(.l, attr, "is_instrument_var") %>% unlist())
  matrices_3 <- mapply(
    "drop_elements",
    object = model_matrices[matrix_list_treatment],
    drop_index = instrument_positions,
    SIMPLIFY = FALSE)

  nice_order <- c("Y","const","const_intra","DX","OX","IX","G")
  return(c(matrices_1,matrices_2,matrices_3)[nice_order])
}
