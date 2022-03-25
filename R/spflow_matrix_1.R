#' @title
#' Generate the design matrices for the spatial interaction model
#'
#' @description
#' Create the model matrices required for matrix-form estimation of
#' the spatial econometric interaction model.
#'
#' @details
#' The key to an efficient estimation is to preserve the relational
#' representation of the data for origins, destinations and
#' origins-destinations pairs.
#' This requires to be aware of the;
#'  - three sources of data (pair, orig, dest)
#'  - three parts of the formula (norm, sdm, inst)
#'  - five roles of the variables (Y_, G_, D_, O_, I_)
#'
#' The additional separation of data sources and roles makes sense if the list
#' of origins coincides with the list of destinations.
#' In this case, we can use data from the same source as origin, destination,
#' or intra-regional characteristics.
#'
#' The model formulas contain information about mathematical transformations
#' of the variables and allow to deduce number of spatial lags required for
#' each of them.
#' To apply the transformations we use R's build-in tools for handling formulas.
#' Spatial lags are calculated after the transformations have been applied.
#' Below is an explanation of the formula parts:
#'  - norm variables are not lagged
#'  - sdm variables are lagged once and used as explanatory variables
#'  - inst variables are lagged twice and used as instruments.
#'   If a variable is at the same time inst and sdm we have to increase the
#'   lags-order to avoid duplicating columns
#'   (see \insertCite{Dargel2021}{spflow}).
#'
#' @references \insertAllCited{}
#' @inheritParams sp_network_pair
#' @inheritParams spflow
#' @name spflow_model_matrix
#' @keywords internal
#' @return A list of design matrices for the spatial interaction model
spflow_model_matrix <- function(
  sp_multi_network,
  network_pair_id,
  flow_formula,
  flow_control,
  ignore_na = FALSE) {

  formula_parts <- interpret_flow_formula(
    flow_formula,
    flow_control)

  # transform by sources = c("pair", "orig", "dest")
  od_data_sources <- pull_relational_flow_data(
    sp_multi_network,
    network_pair_id)

  od_model_matrices <- by_source_model_matrix(
    formula_parts,
    od_data_sources,
    ignore_na = ignore_na)

  # check if the case is cartesian
  #   and if observations are dropped due to the transformations
  source_obs <- unlist(lapply(od_model_matrices, "nrow"))
  trans_obs <- unlist(lapply(od_model_matrices, "nrow"))
  non_cartesian <- source_obs["pair"] < prod(source_obs[c("orig","dest")])
  lost_rows <- !all(unlist(Map("==", source_obs, trans_obs)))

  flow_indicator <- NULL
  if (non_cartesian | lost_rows) {
    pairobs_index <- as.integer(row.names(od_model_matrices[["pair"]]))
    od_keys <- attr_key_od(od_data_sources[["pair"]])
    do_indexes <- od_data_sources[["pair"]][rev(od_keys)]
    do_indexes <- Reduce("cbind", lapply(do_indexes, "as.integer"))
    do_indexes <- do_indexes[pairobs_index,]

    flow_indicator <- matrix_format_d_o(
      dest_index = do_indexes[,1],
      orig_index = do_indexes[,2],
      num_dest = source_obs[["dest"]],
      num_orig = source_obs[["orig"]])
    }


  # transforming from vector to matrix format
  mat_formatter <- switch(EXPR = class(flow_indicator)
  , "ngCMatrix" = {
    imat <- as(flow_indicator,"dgCMatrix")
    function(vec) {
      imat@x <- vec
      return(imat)}}
  , "matrix" = {
    nd <- source_obs[["dest"]]
    no <- source_obs[["orig"]]
    function(vec) {
      mat <- matrix(0, nrow = nd, ncol = no)
      mat[do_indexes] <- vec
      return(mat)}}
  , "NULL" = {
    nd <- source_obs[["dest"]]
    no <- source_obs[["orig"]]
    function(vec) {
      mat <- matrix(vec, nrow = nd, ncol = no)
      return(mat)}})

  # solve constant terms and weights
  weights <- flow_control[["weight_var"]]
  if (!is.null(weights)) {
    weights <- od_data_sources$pair[[weights]]
    if (lost_rows)
      weights <- weights[pairobs_index]
    weights <- mat_formatter(weights)
  }


  od_neighborhoods <- pull_neighborhood_data(sp_multi_network, network_pair_id)
  constants <- derive_flow_constants(
    use_global_const = formula_parts[["constants"]][["global"]],
    use_intra_const = isTRUE(formula_parts[["constants"]][["intra"]]),
    use_instruments = flow_control[["estimation_method"]] == "s2sls",
    flow_indicator = flow_indicator,
    OW = od_neighborhoods[["OW"]],
    DW = od_neighborhoods[["DW"]])

  # compute spatial lags and sort into roles = c("Y_", "D_", "O_", "I_", "G_")
  variable_roles <- define_variable_roles(
    formula_parts,
    lapply(od_data_sources, "subset_keycols" , drop_keys = TRUE))
  model_matrices <- by_role_spatial_lags(
    model_matrices = od_model_matrices,
    variable_roles = variable_roles,
    flow_control = flow_control,
    flow_indicator = flow_indicator,
    neighborhoods = od_neighborhoods,
    mat_formatter = mat_formatter)


  return(c(constants,
           model_matrices,
           od_neighborhoods,
           list("weights" = weights,
                "flow_indicator" = flow_indicator,
                "mat_formatter" = mat_formatter)))
}



#' @keywords internal
pull_pair_o_d_data <- function(
  sp_multi_net,
  pair_id,
  drop_keys,
  only_keys = FALSE) {

  if (missing(drop_keys))
    drop_keys <- !only_keys
  only_keys <- !drop_keys

  source_ids <- as.list(id(sp_multi_net@network_pairs[[pair_id]]))

  # fd = flow_data
  fd <- lapply(source_ids, function(.id) dat(sp_multi_net, .id))

  for (i in seq_along(fd)) {

    if (names(fd)[i] == "pair")
      key_cols <- attr_key_od(fd[[i]])

    if (names(fd)[i] %in% c("orig","dest"))
      key_cols <- c(attr_key_nodes(fd[[i]]), attr_coord_col(fd[[i]]))

    if (only_keys)
      keys <- setdiff(names(fd[[i]]), keys)

    fd[[i]][key_cols] <- NULL
  }

  return(fd)
}


#' @keywords internal
pull_relational_flow_data <- function(
  sp_multi_net,
  pair_id) {

  source_ids <- as.list(id(sp_multi_net@network_pairs[[pair_id]]))
  flow_data <- lapply(source_ids, function(.id){
    source_data <- as.data.frame(dat(sp_multi_net, .id))
    row.names(source_data) <- NULL
    source_data
    })
  return(flow_data)
}

#' @keywords internal
get_keycols <- function(df) {
  c(attr_key_od(df),
    attr_coord_col(df),
    attr_key_nodes(df))
}

#' @keywords internal
subset_keycols <- function(df, drop_keys = TRUE) {
    keep_cols <- get_keycols(df)
  if (drop_keys)
    keep_cols <- setdiff(names(df), keep_cols)
  return(subset(df, select = keep_cols))
}

#' @keywords internal
pull_neighborhood_data <-  function(sp_multi_network, network_pair_id) {

  od_id <- id(sp_multi_network@network_pairs[[network_pair_id]])
  neighbor_mats <- named_list(c("OW","DW"))
  neighbor_mats[["OW"]] <- neighborhood(sp_multi_network, od_id["orig"])
  neighbor_mats[["DW"]] <- neighborhood(sp_multi_network, od_id["dest"])

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
