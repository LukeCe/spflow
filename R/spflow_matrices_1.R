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
#' @name derive_spflow_matrices
#' @keywords internal
#' @return A list of design matrices for the spatial interaction model
derive_spflow_matrices <- function(
  spflow_data,
  spflow_neighborhood,
  spflow_formula,
  spflow_control,
  na_rm = FALSE) {


  formula_parts <- interpret_spflow_formula(
    spflow_formula,
    spflow_control)
  spflow_matrices <- transform_spflow_data(
    formula_parts = formula_parts,
    spflow_data = spflow_data,
    na_rm = na_rm,
    weights_var = spflow_control[["weight_variable"]],
    is_within = spflow_control[["is_within"]])


  variable_usage <- define_lags_and_instruments(
    formula_parts,
    lapply(spflow_data, "subset_keycols" , drop_keys = TRUE))
  spflow_matrices <- lag_spflow_matrices(
    variable_usage = variable_usage,
    spflow_matrices = spflow_matrices,
    spflow_neighborhood = spflow_neighborhood,
    spflow_indicators = spflow_matrices[["spflow_indicators"]],
    model = spflow_control[["model"]],
    decorrelate_instruments = isTRUE(spflow_control[["twosls_decorrelate_instruments"]]),
    reduce_pair_instruments = isTRUE(spflow_control[["twosls_reduce_pair_instruments"]]))


  constants <- list("CONST" = derive_spflow_constants(
    use_global_const = formula_parts[["constants"]][["global"]],
    use_intra_const = isTRUE(formula_parts[["constants"]][["intra"]]),
    use_instruments = spflow_control[["estimation_method"]] == "s2sls",
    spflow_indicators = spflow_matrices[["spflow_indicators"]],
    OW = spflow_neighborhood[["OW"]],
    DW = spflow_neighborhood[["DW"]]))
  return(c(constants, spflow_matrices))
}

#' @keywords internal
pull_spflow_data <- function(
  sp_multi_net,
  pair_id) {

  source_ids <- as.list(id(sp_multi_net@network_pairs[[pair_id]]))
  flow_data <- lapply(source_ids, function(.id){
    source_data <- dat(sp_multi_net, .id)
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
derive_spflow_constants <- function(
    use_global_const,
    use_intra_const,
    use_instruments,
    spflow_indicators = NULL,
    OW = NULL,
    DW = NULL) {


  c_terms <- list("(Intercept)" =  `attr_inst_status<-`(1, !use_global_const))
  if (!use_intra_const)
    return(c_terms)

  n <- nlevels(spflow_indicators[[1]])
  c_terms <- c(c_terms, "(Intra)" = `attr_inst_status<-`(Diagonal(n), FALSE))
  if (!use_instruments)
    return(c_terms)

  # for computation of instruments the indicator is based on observed Y
  Y_indicator <- spflow_indicators2mat(spflow_indicators, do_filter = "HAS_Y")
  intra_lags <- double_lag_matrix(
    M = Diagonal(n),
    OW = OW,
    DW = DW,
    name = "(Intra)",
    key = "I",
    M_indicator = Y_indicator,
    symmetric_lags = is.null(Y_indicator),
    lag_order = 2,
    return_all_lags = TRUE,
    lags_are_instruments = TRUE)
  return(c(c_terms, intra_lags[-1]))
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
