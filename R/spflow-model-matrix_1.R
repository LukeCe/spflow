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
#' @name spflow_model_matrix
#' @keywords internal
#' @return A list of design matrices for spatial interaction model
spflow_model_matrix <- function(
  sp_multi_network,
  network_pair_id,
  formula_parts,
  estim_control
) {

  # transform original variables according to formula
  data_sources <- pull_flow_data(sp_multi_network, network_pair_id)
  model_matrices <- by_source_model_matrix(formula_parts, data_sources)

  # generate the required lags and split the variables according to their
  # roles (O_ vs D_ vs I_ ...)
  variable_roles <- define_variable_roles(formula_parts, data_sources)
  neighborhoods <- pull_neighborhood_data(sp_multi_network, network_pair_id)
  model_matrices <- by_role_spatial_lags(
    model_matrices = model_matrices,
    variable_roles = variable_roles,
    neighborhoods = neighborhoods,
    estim_control)

  # Extract weights and constants if they are defined
  constants <- define_flow_constants(
    const_formula = formula_parts[["constants"]],
    use_instruments = estim_control[["estimation_method"]] == "s2sls",
    OW = neighborhoods[["OW"]])


  # weights matter if they are specified or if the flows are not complete
  weights <- estim_control[["weight_var"]]
  if (!is.null(weights))
    weights <- estim_control$mat_format(data_sources$pair[[weights]])
  if (is.null(weights) && estim_control[["mat_complet"]] < 1)
    weights <- estim_control$mat_format(1)

  return(c(model_matrices, neighborhoods,
           list("constants" = constants,
                "weights" = weights)))
}


#' @keywords internal
pull_flow_data <- function(sp_multi_net, pair_id) {

  # identification of the data sources
  od_id <- split_pair_id(pair_id)
  source_ids <- list("pair" = pair_id, "orig" = od_id[1], "dest" = od_id[2])

  if (has_equal_elements(od_id))
    source_ids[["dest"]] <- NULL

  flow_data <- lapply(
    source_ids,
    function(.id) as.data.frame(dat(sp_multi_net, .id)))
  for (i in seq_along(flow_data)) {
    if (names(flow_data)[i] == "pair")
      key_cols <- attr_key_od(flow_data[[i]])

    if (names(flow_data)[i] %in% c("orig","dest"))
      key_cols <- attr_key_nodes(flow_data[[i]])

    flow_data[[i]][key_cols] <- NULL
  }


  return(flow_data)
}

#' @keywords internal
pull_neighborhood_data <-  function(sp_multi_network, network_pair_id) {

  od_id <- split_pair_id(network_pair_id)
  neighbor_mats <- named_list(c("OW","DW"))
  neighbor_mats[["OW"]] <-
    neighborhood(sp_multi_network, od_id[1])
  neighbor_mats[["DW"]] <-
    neighborhood(sp_multi_network, od_id[2])

  return(compact(neighbor_mats))

}

#' @title Internal functions to generate model matrices
#' @details
#'   The function generates a list for each role `c("Y_","G_","O_","D_","I_")`
#'   which indicates whether a variable in the design matrix is used as
#'   `c("norm", "sdm", "inst")`.
#' @return
#'   A list of lists, with information on the way variables are used
#'   in the model. (e.g. origin vs destination and the number of lags)
#' @keywords internal
define_variable_roles <- function(formula_parts, data_sources) {

  roles <- c("Y_","G_","O_","D_","I_")
  is_within <- !"dest" %in% names(data_sources)

  source_role_lookup <- roles_to_sources(is_within)
  role_usage <- translist(formula_parts)[roles]
  role_usage <- compact(role_usage)

  what_vars_are_lagged_roles <- function(role_key) {
    source_key <- source_role_lookup[role_key]
    lapply(role_usage[[role_key]], "predict_tranfomed_vars",
           data_sources[[source_key]])
  }
  role_lags <- lapply(lookup(names(role_usage)), "what_vars_are_lagged_roles")
  role_lags <- compact(role_lags)

  # remove lags for Y_ that appear in the G_ part
  dependent_vars <- role_lags[["Y_"]][["norm"]]
  role_lags[["G_"]][["norm"]] <-
    setdiff(role_lags[["G_"]][["norm"]], dependent_vars)
  role_lags[["G_"]][["inst"]] <-
    setdiff(role_lags[["G_"]][["inst"]], dependent_vars)

  return(role_lags)
}

#' @keywords internal
roles_to_sources <- function(is_within) {
  D_source <- if (is_within) "orig" else "dest"
  c("Y_" = "pair",
    "G_" = "pair",
    "O_" = "orig",
    "D_" = D_source,
    "I_" = "orig")
}

#' @keywords internal
define_flow_constants <- function(const_formula, use_instruments, OW = NULL) {

  global_const <- `attr_inst_status<-`(1, FALSE) %T% const_formula[["global"]]
  intra_const <- NULL
  if (isTRUE(const_formula[["intra"]]))
    intra_const <- intra_regional_constant(OW, use_instruments)

  return(list("global" = global_const, "intra" = intra_const))
}


#' @importFrom Matrix Diagonal tcrossprod t
#' @keywords internal
intra_regional_constant <- function(W, use_instruments = FALSE) {

  In <- Diagonal(nrow(W))
  attr_inst_status(In) <- FALSE
  In <- list("(Intra)" = In)
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
  )
  w_int <- lapply(w_int, "attr_inst_status<-",TRUE)

  return(c(In,w_int))
}
