#' Estimate spatial interaction models that incorporate spatial dependence
#'
#' @description
#' We implement three different estimators of spatial interaction models that
#' allow to estimate origin-destination flows with spatial auto-correlation.
#'
#' The function allows to include spatial lags of the explanatory, which will
#' corresponds to a so called Spatial Durbin model (SDM)
#' \insertCite{Anselin1988;textual}{spflow}.
#' Moreover, we give the option to include an additional set of coefficients
#' for intra-regional flows (origin = destination), as proposed by
#' \insertCite{LeSage2009;textual}{spflow}.
#' Both options are activated by default and can be adjusted through the
#' [spflow_control()] object.
#'
#' @details
#' Our estimation procedures make use of the matrix formulation introduced by
#' \insertCite{LeSage2008;textual}{spflow} and further developed by
#' (#Dargel2020Improve) to reduce the computational effort and memory
#' requirements considerably.
#' See (#Dargel2020Sim) for an exhaustive simulation study.
#' The estimation procedure can be adjusted through the estimation_method
#' in [spflow_control()].
#'
#' @section Maximum likelihood estimation (MLE):
#' Maximum likelihood estimation is the default estimation procedure.
#' The estimation was first developed by
#' \insertCite{LeSage2008;textual}{spflow} and then improved by
#' (#Dargel2020Improve).
#'
#' @section Spatial two-stage least squares (S2SLS):
#' The S2SLS estimator if an adaption of the one proposed by
#' \insertCite{Kelejian1998;textual}{spflow}, to the case origin-destination
#' flows, with a double spatial index and up to three neighborhood matrices
#' (#Dargel2020Improve).
#' A similar estimation is done by \insertCite{Tamesue2016;textual}{spflow}.
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
#' models, which has the following structure:
#' `Y ~ O_(X1) + D_(X2) + I_(X3) + G_(X4)`
#' This structure reflects the different data sources involved in such a model.
#' On the left hand side is the independent variable `Y` which corresponds to
#' the vector of flows.
#' On the right hand side we have all the explanatory variables.
#' The functions `O_(...)` and `D_(...)` indicate which variables are used as
#' characteristics of the origins and destinations respectively.
#' Similarly, `I_(...)` indicates variables that should be used for the
#' intra-regional coefficients.
#' Finally, `G_(...)` declares which variables describe origin-destination
#' pairs, which most frequently will be a measure of distance.
#'
#' All the declared variables must be available in the provided
#' [sp_multi_network()], which gathers information on the origins and
#' destinations [sp_network_nodes()] as well as the origin-destination pairs
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
         "The data must be a network data object!")

  ## ... check correct types of ids
  assert(is_single_character(network_pair_id),
         "The network_pair_id must be a character of length 1!")

  network_ids <- id(sp_multi_network)[["network_pairs"]][[network_pair_id]]
  assert(!is.null(network_ids),
         "The the network pair id [%s] is not available!" %>%
           sprintf(., network_pair_id))



  ## ... test the arguments provided to control by calling it again
  flow_control <- do.call(spflow_control, flow_control)
  # TODO validate and enrich flow control
  ## ... identify the flow type
  flow_control$flow_type <- ifelse(
    network_ids["orig"] ==
      network_ids["dest"],
    yes = "within", no = "between"
  )

  ## ... create the design matrix/matrices
  model_matrices <- spflow_model_matrix(
    sp_multi_network,
    network_pair_id,
    flow_formula,
    flow_control)

  # ... fit the model and add complementary information to the results
  estimation_results <- spflow_model_estimation(model_matrices,flow_control)

  return(estimation_results)
}



#' @keywords internal
parameter_names <- function(
  model_matrices,
  model) {

  names_rho <- identify_auto_regressive_parameters(model)
  names_const <- c("Constant", "Constant_intra")
  use_const <- c(model_matrices$constants$global == 1,
                 !is.null(model_matrices$constants$intra$In))
  names_const <- names_const[use_const]

  x_prefs <- list("D_" = "Dest_","O_" = "Orig_","I_" = "Intra_")
  names_X <- model_matrices[names(x_prefs)] %>% compact() %>%
    lapply("colnames") %>% plapply(x_prefs[names(.)],., .f = "%p%") %>%
    flatten(use.names = FALSE)

  names_G <- names(model_matrices$G)
  export_names <- c(names_rho,names_const,names_X,names_G)

  return(export_names)

}

#' @keywords internal
drop_instruments <- function(model_matrices) {

  # ... from constants
  filter_inst <- function(x) lfilter(x, function(x) !get_instrument_status(x))
  constants <- list(
    "global" = model_matrices$constants$global,
    "intra" = model_matrices$constants$intra %>% filter_inst())

  # ... from site attributes
  filter_inst_col <- function(x) cols_drop(x,get_instrument_status(x))
  vector_treatment <- c("D","O","I") %p% "_"
  matrices_X <- model_matrices[vector_treatment] %>%
    compact() %>% lapply("filter_inst_col")

  # ... from pair attributes
  matrices_G <- model_matrices["G_"] %>% lapply("filter_inst")

  # ... combine cleaned versions
  matrices_and_spatial_weights <- c(
    model_matrices["Y_"],
    list("constants" = constants),
    matrices_X,
    matrices_G,
    model_matrices[c("DW","OW")] %>% compact()
  )

  return(matrices_and_spatial_weights)
}
