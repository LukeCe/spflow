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
  flow_formula,
  flow_control) {

  formula_parts <- interpret_flow_formula(
    flow_formula,
    flow_control)

  # 1. transform by sources = c("pair", "orig", "dest")
  od_data_sources <- pull_relational_flow_data(
    sp_multi_network,
    network_pair_id)

  od_model_matrices <- by_source_model_matrix(
    formula_parts,
    od_data_sources)

  # 2. solve constant terms, weight and indicator
  weights <- flow_control[["weight_var"]]
  if (!is.null(flow_control[["weight_var"]]))
    weights <- flow_control$mat_format(od_data_sources$pair[[weights]])

  flow_indicator <- NULL
  if (flow_control[["mat_complet"]] < 1)
    flow_indicator <- flow_control$mat_format(NULL)

  od_neighborhoods <- pull_neighborhood_data(sp_multi_network, network_pair_id)
  constants <- derive_flow_constants(
    use_global_const = formula_parts[["constants"]][["global"]],
    use_intra_const = isTRUE(formula_parts[["constants"]][["intra"]]),
    use_instruments = flow_control[["estimation_method"]] == "s2sls",
    flow_indicator = flow_indicator,
    OW = od_neighborhoods[["OW"]],
    DW = od_neighborhoods[["DW"]])

  # 3. compute spatial lags and sort into roles = c("Y_", "D_", "O_", "I_", "G_")
  variable_roles <- define_variable_roles(formula_parts, od_data_sources)
  model_matrices <- by_role_spatial_lags(
    model_matrices = od_model_matrices,
    variable_roles = variable_roles,
    flow_control = flow_control,
    flow_indicator = flow_indicator,
    neighborhoods = od_neighborhoods)


  return(c(constants,
           model_matrices,
           od_neighborhoods,
           list("weights" = weights,
                "flow_indicator" = flow_indicator)))
}


#' @keywords internal
pull_relational_flow_data <- function(sp_multi_net, pair_id) {

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

#' @importFrom Matrix Diagonal
#' @keywords internal
derive_flow_constants <- function(
    use_global_const,
    use_intra_const,
    use_instruments,
    flow_indicator = NULL,
    OW = NULL,
    DW = NULL) {

  if (!use_global_const & !use_intra_const)
    return(NULL)

  c_terms <- named_list("const","const_intra")
  c_terms[["const"]] <- `attr_inst_status<-`(1, FALSE) %T% use_global_const

  if (!use_intra_const)
    return(c_terms["const"])

  In <- Diagonal(nrow(OW), diag(flow_indicator) %||% 1)
  attr_inst_status(In) <- FALSE
  c_terms[["const_intra"]] <- list("(Intra)" = In)

  if (!use_instruments)
    return(c_terms)

  c_terms[["const_intra"]] <- double_lag_matrix(
    M = In,
    OW = OW,
    DW = DW,
    name = "(Intra)",
    key = "I",
    flow_indicator = flow_indicator,
    symmetric_lags = is.null(flow_indicator),
    lag_order = 2,
    return_all_lags = TRUE,
    lags_are_instruments = TRUE
  )


  return(c_terms)
}
