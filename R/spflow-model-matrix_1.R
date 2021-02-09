#' @title
#' Generate design matrices for the spatial interaction model
#'
#' @description
#' Creates model matrices from a [sp_multi_network()] network.
#' For efficiency the relational representation of origin and destination data
#' is preserved.
#'
#' @inheritParams sp_network_pair
#' @inheritParams spflow
#'
#' @keywords internal
#' @return A list of design matrices for spatial interaction model
spflow_model_matrix <- function(
  sp_multi_network,
  network_pair_id,
  flow_formula,
  flow_control
) {

  formula_parts <- interpret_flow_formula(flow_formula, flow_control)

  # transform original variables
  data_sources <- pull_flow_data(sp_multi_network, network_pair_id)
  model_matrices <- by_source_model_matrix(formula_parts, data_sources)

  # prepare arguments for creation of spatial lags and matrix forms
  lag_requirements <- def_spatial_lag_requirements(formula_parts, data_sources)
  matrix_infos <- def_matrix_form_args(sp_multi_network, network_pair_id)
  neighborhoods <- pull_neighborhood_data(sp_multi_network, network_pair_id)

  # generate the spatial lags by source then split by role
  model_matrices <- by_role_spatial_lags(
    source_model_matrices = model_matrices,
    lag_requirements = lag_requirements,
    neighborhoods = neighborhoods,
    matrix_form_arguments = matrix_infos,
    model = flow_control$model,
    decorrelate_instruments = flow_control$decorrelate_instruments,
    reduce_pair_instruments = flow_control$reduce_pair_instruments)

  # Extract weights and constants if they are defined
  constants <- define_flow_constants(
    const_formula = formula_parts$const,
    use_instruments = flow_control$estimation_method == "s2sls",
    OW = neighborhoods$OW)

  weights <- define_flow_weights(data_sources$pair,
                                 flow_control$weight_var,
                                 matrix_infos)

  return(c(model_matrices, neighborhoods,
           list("constants" = constants, "weights" = weights)))
}


#' @importFrom data.table key copy
#' @keywords internal
pull_flow_data <- function(sp_multi_network, network_pair_id) {

  # identification of the data sources
  data_source_ids <- id(sp_multi_network)$network_pairs[[network_pair_id]]
  pair_id <- data_source_ids["pair"]
  orig_id <- data_source_ids["orig"]
  dest_id <- data_source_ids["dest"]

  orig_data <- pull_nodes(sp_multi_network,orig_id)
  dest_data <- pull_nodes(sp_multi_network,dest_id) %T% (orig_id != dest_id)
  pair_data <- pull_pairs(sp_multi_network,pair_id)

  flow_data <- list("orig" = orig_data,
                    "dest" = dest_data,
                    "pair" = pair_data) %>% compact() %>%
    lapply("dat") %>% copy()

  # define completeness and extract the keys
  flow_data <- flow_data %>% lapply(function(.d) cols_drop(.d,key(.d)))

  return(flow_data)
}

#' @keywords internal
pull_neighborhood_data <-  function(sp_multi_network, network_pair_id) {

  # identification of the data sources
  data_source_ids <- id(sp_multi_network,"network_pairs") %[[% network_pair_id
  orig_id <- data_source_ids["orig"]
  dest_id <- data_source_ids["dest"]

  neighborhoods <- pull_neighborhood(sp_multi_network, c(orig_id, dest_id))
  names(neighborhoods) <- c("OW","DW")
  return(neighborhoods)

}

#' @keywords internal
def_matrix_form_args <- function(sp_multi_network, network_pair_id) {

  sp_pair <- pull_pairs(sp_multi_network,network_pair_id)
  flow_completeness <- list(
    "completeness" = prod(nnodes(sp_pair)) / npairs(sp_pair),
    "n_rows" = nnodes(sp_pair,"orig"),
    "n_cols" = nnodes(sp_pair,"dest"),
    "i_rows" = sp_pair %>% dat() %[[% "ORIG_ID" %>% as.integer(),
    "j_cols" = sp_pair %>% dat() %[[% "DEST_ID" %>% as.integer())


  return(flow_completeness)

}



#' @keywords internal
def_spatial_lag_requirements <- function(formula_parts, data_sources) {

  # variable usage by roles
  roles <- c("Y_","G_","O_","D_","I_")
  is_within <- !is.null(data_sources)
  source_role_lookup <- roles_to_sources(is_within)
  role_usage <- formula_parts %>% translist() %[% roles %>% compact()

  what_vars_are_lagged_roles <- function(role_key) {
    source_key <- source_role_lookup[role_key]
    lapply(role_usage[[role_key]], "predict_tranfomed_vars",
           data_sources[[source_key]])
  }
  role_lags <- lookup(names(role_usage)) %>%
    lapply(what_vars_are_lagged_roles) %>% compact()

  # remove lags for Y_ where it does not belong...
  dependent_vars <- role_lags$Y_$norm
  role_lags$G_$norm <- setdiff(role_lags$G_$norm, dependent_vars)
  role_lags$G_$inst <- setdiff(role_lags$G_$inst, dependent_vars)

  return(role_lags)
}

#' @keywords internal
roles_to_sources <- function(is_within) {
  D_source <- if (is_within) "orig" else "dest"
  c("Y_" = "pair","G_" = "pair", "O_" = "orig","D_" = D_source, "I_" = "orig")
}

#' @importFrom Matrix t
#' @keywords internal
define_flow_constants <- function(const_formula, use_instruments, OW = NULL) {

  global_const <-
    (1 %>%  set_instrument_status(FALSE)) %T% const_formula$global

  intra_const <- NULL
  if (isTRUE(const_formula$intra))
    intra_const <- intra_regional_constant(OW, use_instruments)

  return(list("global" = global_const, "intra" = intra_const))
}


#' @importFrom Matrix Diagonal tcrossprod
#' @keywords internal
intra_regional_constant <- function(W, use_instruments = FALSE) {

  In <- list("In" =  Diagonal(nrow(W)) %>% set_instrument_status(FALSE))
  if (!use_instruments)
    return(In)

  V <- tcrossprod(W) # def. V = WW'
  WV <- W %*% V
  WW <- W %*% W
  w_int <- list(
    "W"   = W,
    "W'"  = t(W),
    "WW"  = WW,
    "WW'" = t(WW),
    "V"   = V,
    "VV"  = tcrossprod(WV, W),
    "WV"  = WV,
    "VW'" = t(WV)
  ) %>% lapply("set_instrument_status",TRUE)

  return(c(In,w_int))
}

#' @keywords internal
define_flow_weights <- function(pair_data, weight_var, matrix_form_arguments){

  # When the flows are incomplete and weights weights are not defined, they
  # are used as indicators.
  weights <- weight_var %|!|% (pair_data %[[% weight_var)
  complete_flows <- matrix_form_arguments$completeness == 1
  weights_dont_matter <- is.null(weights) & complete_flows
  if (weights_dont_matter)
    return(NULL)

  args <- c(list("vec" = weights),matrix_form_arguments)
  weights_mat <- do.call(vec_to_matrix,args = args)

  return(weights_mat)
}
