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


  od_data_sources <- pull_relational_flow_data(
    sp_multi_network,
    network_pair_id)

  od_neighborhoods <- pull_neighborhood_data(
    sp_multi_network = sp_multi_network,
    network_pair_id = network_pair_id)

  od_neighborhoods <- valdiate_od_neighborhoods(
    od_neighborhoods = od_neighborhoods,
    model = flow_control[["model"]],
    do_normalisation = flow_control[["neighborhood_do_normalisation"]])

  formula_parts <- interpret_flow_formula(
    flow_formula,
    flow_control)

  flowmodel_matrices <- flowdata_transformations(
    formula_parts = formula_parts,
    data_sources = od_data_sources,
    na_rm = ignore_na,
    weights_var = flow_control[["weights"]])

  variable_usage <- define_lags_and_instruments(
    formula_parts,
    lapply(od_data_sources, "subset_keycols" , drop_keys = TRUE))

  flowmodel_matrices <- flowdata_spatiallag(
    variable_usage = variable_usage,
    flowmodel_matrices = flowmodel_matrices,
    nb_matrices = od_neighborhoods,
    model = flow_control[["model"]],
    decorrelate_instruments = isTRUE(flow_control[["twosls_decorrelate_instruments"]]),
    reduce_pair_instruments = isTRUE(flow_control[["twosls_reduce_pair_instruments"]]))

  constants <- derive_flow_constants(
    use_global_const = formula_parts[["constants"]][["global"]],
    use_intra_const = isTRUE(formula_parts[["constants"]][["intra"]]),
    use_instruments = flow_control[["estimation_method"]] == "s2sls",
    flow_indicator = flowmodel_matrices[["flow_indicator"]],
    OW = od_neighborhoods[["OW"]],
    DW = od_neighborhoods[["DW"]])


  return(c(constants, flowmodel_matrices))
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
  c(attr_key_do(df),
    attr_key_nodes(df),
    attr_coord_col(df))
}

#' @keywords internal
subset_keycols <- function(df, drop_keys = TRUE) {
    keep_cols <- get_keycols(df)
  if (drop_keys)
    keep_cols <- setdiff(names(df), keep_cols)
  return(df[, keep_cols, drop = FALSE])
}

#' @keywords internal
pull_neighborhood_data <-  function(sp_multi_network, network_pair_id) {

  od_id <- id(sp_multi_network@network_pairs[[network_pair_id]])
  neighbor_mats <- lapply(c("OW" = "orig", "DW" = "dest"), function(.key) {
    m <- neighborhood(sp_multi_network, od_id[.key])
    dimnames(m) <- list(NULL,NULL)
    return(m)
  })

  return(compact(neighbor_mats))
}

#' @keywords internal
define_lags_and_instruments <- function(formula_parts, data_sources) {

  formulas2sources  <- c(
    "D_" = "dest",
    "O_" = "orig",
    "I_" = "orig",
    "G_" = "pair",
    "Y_" = "pair")


  interpret_formulas <- function(.key_form) {
    .key_source <- formulas2sources[.key_form]
    derive_variables_use(variable_usage[[.key_form]], data_sources[[.key_source]])
  }

  variable_usage <- translist(formula_parts)[names(formulas2sources)]
  variable_usage <- compact(variable_usage)
  variable_usage <- lapply(lookup(names(variable_usage)), interpret_formulas)
  return(variable_usage)
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

  c_terms <- named_list("const","const_intra")
  c_terms[["const"]] <- `attr_inst_status<-`(1, !use_global_const)

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


#' @keywords internal
derive_variables_use <- function(
    formula_part,
    data_source) {

  var_use <- lapply(formula_part, "predict_tranfomed_vars", data_source)
  var_use_types <- named_list(c("norm","sdm","inst"))
  var_use_types[names(var_use)] <- var_use

  all_vars <- unique(unlist(var_use, use.names = FALSE))
  get_v_use <- function(.v) data.frame(lapply(var_use_types, function(.u) any(.u == .v)))
  var_use_df <- do.call("rbind", lapply(lookup(all_vars), get_v_use))
  var_use_df[["num_lags"]] <- var_use_df[["sdm"]] + 2 * var_use_df[["inst"]]

  inst_attr <- Map(
    function(norm, sdm, num_lags){

      inst_statu <- rep(TRUE, num_lags + 1)
      inst_statu[1] <- !norm
      if(num_lags >= 1)
        inst_statu[2] <- !sdm
      inst_statu
    },
    var_use_df$norm, var_use_df$sdm, var_use_df$num_lags)

  var_use_df[["inst_attr"]] <- I(inst_attr)
  var_use_df
}

#' @keywords interal
valdiate_od_neighborhoods <- function(
  od_neighborhoods,
  model,
  do_normalisation = TRUE) {

  model_num <- as.numeric(substr(model,7,7))
  req_OW <- model_num %in% c(3:9)
  req_DW <- model_num %in% c(2,4:9)

  assert(!req_OW  || !is.null(od_neighborhoods[["OW"]]),
         "For model_%s you the origin neighborhood musst be available!",
         model_num)
  assert(!req_DW || !is.null(od_neighborhoods[["DW"]]),
         "For model_%s you the destination neighborhood musst be available!",
         model_num)


  spectral_radi <- lapply(od_neighborhoods, function(.XW) abs(attr_spectral_character(.XW)["LM"]))
  tol <- sqrt(.Machine$double.eps)
  unit_radi <- all(abs(unlist(spectral_radi) - 1) < tol)
  if (unit_radi)
    return(od_neighborhoods)

  row_sums <- lapply(od_neighborhoods, rowSums)
  row_sums_0or1 <- lapply(row_sums, function(.rs) all(abs(abs(.rs - .5) - .5) < tol))
  row_normalized <- all(unlist(row_sums_0or1))
  if (row_normalized)
    return(od_neighborhoods)

  assert(do_normalisation, "The neighborhood matrices should be normalized!")
  od_neighborhoods <- Map(
    function(.W, .sr) {
      .W <- .W / .sr
      attr_spectral_character(.W) <- attr_spectral_character(.W) / .sr
      .W
    },
    .W = od_neighborhoods,
    .sr = spectral_radi)

  return(od_neighborhoods)
}

