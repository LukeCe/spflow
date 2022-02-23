#' @keywords internal
by_role_spatial_lags <- function(
  model_matrices,
  variable_roles,
  neighborhoods,
  estim_control,
  flow_indicator){

  ### 0) Define requirements
  instrument_statu_by_role <-
    lapply(variable_roles, "predict_lags_and_inst_satus")
  lag_requirements_by_role <-
    lapply(instrument_statu_by_role, "lapply",
           function(.n) lookup(names(.n)))

  summarize_lags_by_source <- function(source_key){
    role_key <- role_lookup[[source_key]]
    result <- lapply(translist(lag_requirements_by_role[role_key]), "unlist")
    lapply(result, "unique")
  }
  sources <- intersect(c("pair", "orig", "dest"), names(model_matrices))
  sources <- lookup(sources)
  role_lookup <- sources_to_roles(!"dest" %in% sources)
  lag_requirements_by_source <- lapply(sources, "summarize_lags_by_source")

  ### 1) Lag data for nodes
  node_sources <- intersect(c("orig","dest"), names(model_matrices))
  lag_varnames_by_role <- lapply(lag_requirements_by_role, "suffix_sp_lags")
  lag_varnames_by_role <- lapply(lag_varnames_by_role, "unlist")
  role_prefixes <- list("D_" = "DEST_","O_" = "ORIG_","I_" = "INTRA_")

  apply_lags_to_node_source <- function(source_key) {

    nb_key <- c("orig" = "OW", "dest" = "DW")[source_key]
    source_nb <- neighborhoods[[nb_key]]
    source_mat <- model_matrices[[source_key]]
    lags <- lag_requirements_by_source[[source_key]]
    lags_names <- unlist(suffix_sp_lags(lags))

    # create one matrix for each source
    lagged_vars_mat <- as.matrix(Reduce(
      function(.x1, .x2) { cbind(.x2, source_nb %*% .x1) },
      lapply(rev(lags), function(.vars) source_mat[,.vars, drop = FALSE])))
    colnames(lagged_vars_mat) <- lags_names

    # split the matrix by roles and declare instruments
    role_keys <- role_lookup[[source_key]]
    mat_by_role <- lapply(
      compact(lag_varnames_by_role[role_keys]),
      function(.vars) lagged_vars_mat[,.vars, drop = FALSE])

    inst_status <- lapply(compact(instrument_statu_by_role[role_keys]),
                          "unlist", use.names = FALSE)

    for (i in seq_along(mat_by_role)) {
      attr_inst_status(mat_by_role[[i]]) <- inst_status[[i]]
      colnames(mat_by_role[[i]]) <-
        role_prefixes[names(mat_by_role)[i]] %p% colnames(mat_by_role[[i]])
    }
    return(mat_by_role)
    }
  node_lags <- flatlist(lapply(node_sources, "apply_lags_to_node_source"))

  if (isTRUE(estim_control[["twosls_decorrelate_instruments"]]))
    node_lags <- lapply(node_lags, "orthoginolize_instruments")

  ### 2) Lag data for pairs
  # ... Y_ lags
  response_variables <-
    unlist(lag_requirements_by_role[["Y_"]], use.names = FALSE)
  flow_matrices <- lapply(
    lookup(response_variables),
    function(.col) estim_control$mat_format(model_matrices[["pair"]][,.col]))

  flow_matrices <- Map(
    "lag_flow_matrix",
    Y = flow_matrices,
    name = response_variables,
    MoreArgs = c(
      estim_control["model"],
      neighborhoods, # OW and DW
      list("flow_indicator" = flow_indicator)))

  # ... G_ lags
  pair_covariates <-
    unique(unlist(lag_requirements_by_role[["G_"]], use.names = FALSE))
  covariate_matrices <- lapply(
    lookup(pair_covariates),
    function(.col) estim_control$mat_format(model_matrices[["pair"]][,.col]))

  lag_pair_covariate <- function(var){
    var_lags <-
      length(Filter(function(x) x == var, lag_requirements_by_role[["G_"]]))
    G_lags <- derive_pair_instruments(
      G = covariate_matrices[[var]],
      OW = neighborhoods[["OW"]],
      DW = neighborhoods[["DW"]],
      name = var,
      full_inst = !isTRUE(estim_control[["twosls_reduce_pair_instruments"]])
    ) %T% (var_lags > 1) %||% covariate_matrices[var]

    inst_a <- unlist(lapply(instrument_statu_by_role[["G_"]], "[", var))
    inst <- !logical(length(G_lags))
    inst[seq_along(inst_a)] <- inst_a
    Map("attr_inst_status<-", x = G_lags,value = inst)
  }
  lagged_covariate_matrices <- lapply(pair_covariates, "lag_pair_covariate")

  ### 3) Combine
  names(flow_matrices) <- NULL
  all_model_matrices <- c(
    list("Y_" = flatlist(flow_matrices),
         "G_" = flatlist(lagged_covariate_matrices)),
    flatlist(node_lags))

  return(all_model_matrices)
}

# ---- Helpers 1: defining requirements ---------------------------------------

#' @keywords internal
predict_lags_and_inst_satus <- function(.vars) {

  # pull out variables and declare their instrument status
  norm <- .vars$norm
  sdm <- .vars$sdm
  inst <- .vars$inst

  inst0 <- setdiff(inst, norm)
  inst0 <- setdiff(inst0, sdm)
  inst1 <- setdiff(inst, sdm)
  inst2 <- setdiff(inst, inst0)
  inst3 <- setdiff(inst, inst0)
  inst3 <- intersect(inst3, sdm)

  i <- function(.var) .var %|!|% lookup(TRUE,.var)
  ni <- function(.var) .var %|!|% lookup(FALSE,.var)
  required_lags <- list(
    "lag0" = c(ni(norm), i(inst0)),
    "lag1" = c(ni(sdm), i(inst1)),
    "lag2" = i(inst2),
    "lag3" = i(inst3)
  )
  required_lags <- lapply(compact(required_lags), "sort_names")
  return(required_lags)
}

#' @keywords  internal
sources_to_roles <- function(is_within) {
  D_ <- "D_" %T% is_within
  compact(list(
    "pair" = c("Y_", "G_"),
    "orig" = c("O_", D_, "I_"),
    "dest" = "D_" %T% (!is_within)
  ))
}

#' @keywords internal
suffix_sp_lags <- function(lag_req) {
  suffix <- c(lag0 = "", lag1 = ".lag1", lag2 = ".lag2", lag3 = ".lag3")
  Map("paste0",lag_req, suffix[names(lag_req)])
}

#' @keywords internal
`attr_inst_status<-` <- function(x, value) {
  attr(x, "is_instrument_var") <- value
  x
}

#' @keywords internal
attr_inst_status <- function(x) {
  attr(x, "is_instrument_var")
}

# ---- Helpers 2: lagging node data -------------------------------------------

#' @keywords internal
orthoginolize_instruments <- function(mat) {

  inst_index <- attr_inst_status(mat)
  no_instruments <- none(inst_index)
  if (no_instruments)
    return(mat)

  vars <- mat[,!inst_index, drop = FALSE]
  inst_orth <- decorellate_matrix(mat[,inst_index, drop = FALSE],
                                  cbind(1,vars))
  inst_orth <- linear_dim_reduction(inst_orth, var_threshold = 1e-4)

  new_matr <- cbind(vars,inst_orth)
  new_inst_order <- Map("rep", c(FALSE,TRUE), c(ncol(vars),ncol(inst_orth)))
  attr_inst_status(new_matr) <- unlist(new_inst_order)
  return(new_matr)
}


# ---- Helpers 3: lagging pair data -------------------------------------------
#' @keywords internal
lag_flow_matrix <- function(
  Y,
  model,
  OW,
  DW,
  name = "Y",
  flow_indicator = NULL) {


  if (model == "model_1")
    return(named_list(name, Y))

  names_rho <- define_spatial_lag_params(model)
  need_d <- any(names_rho %in% c("rho_d","rho_od","rho_odw"))
  need_o <- any(names_rho %in% c("rho_o","rho_od","rho_odw"))
  need_w <- any(names_rho %in% c("rho_w","rho_odw"))

  if (need_d)
    WY <- DW %*% Y
  if (need_o)
    YW <- tcrossprod(Y,OW)
  # The two sided lag may exploit the existing one-sided ones for efficiency
  if (need_w) {
    if (need_d)
      WYW <- tcrossprod(WY, OW)
    if (!need_d & need_o)
      WYW <- DW %*% YW
    if (!need_d & !need_o)
      WYW <-  DW %*% tcrossprod(Y,OW)
  }

  Y_lags <- switch(substr(model, 7, 7),   # (8.15) in LeSage book
                   "9" = list(" " = Y, ".d"   = WY, ".o" = YW, ".w" = WYW),
                   "8" = list(" " = Y, ".d"   = WY, ".o" = YW, ".w" = WYW),
                   "7" = list(" " = Y, ".d"   = WY, ".o" = YW),
                   "6" = list(" " = Y, ".odw" = (WY + YW + WYW)/3),
                   "5" = list(" " = Y, ".od"  = (WY + YW)/2),
                   "4" = list(" " = Y, ".w"   = WYW),
                   "3" = list(" " = Y, ".o"   = YW),
                   "2" = list(" " = Y, ".d"   = WY),
                   "1" = list(" " = Y))
  names(Y_lags) <- strwrap(paste0(name, names(Y_lags)))
  if (!is.null(flow_indicator) & length(Y_lags) > 1)
    Y_lags[-1] <- lapply(Y_lags[-1], "*", flow_indicator)


  return(Y_lags)
}

#' @keywords internal
define_spatial_lag_params <- function(model) {
  names_rho <- switch(substr(model, 7, 7),
                      "9" = c("rho_d", "rho_o", "rho_w"),
                      "8" = c("rho_d", "rho_o", "rho_w"),
                      "7" = c("rho_d", "rho_o"),
                      "6" = "rho_odw",
                      "5" = "rho_od",
                      "4" = "rho_w",
                      "3" = "rho_o",
                      "2" = "rho_d",
                      "1" = NULL)

  return(names_rho)
}

#' @keywords internal
derive_pair_instruments <- function(
  G,
  OW,
  DW,
  name = "G",
  full_inst = FALSE,
  flow_indicator = NULL) {

  if (is.null(G))
    return(NULL)

  if (is.null(OW) & is.null(DW))
    return(named_list(name,G))

  if (is.null(OW) | is.null(DW))
    full_inst <- TRUE

  include_index <-
    if (!is.null(flow_indicator)) do_nothing else function(x) x * flow_indicator


  lag_left <- lag_right <- lag_double <- !is.null(OW) & !is.null(DW)
  lag_left <- !is.null(DW) & full_inst
  lag_right <- !is.null(OW) & full_inst

  lags_o1 <- named_list(c("wG", "Gw", "wGw"))
  lags_o1[["wG"]]  <- (DW %*% G) %T% lag_left
  lags_o1[["Gw"]]  <- tcrossprod(G, OW) %T% lag_right
  lags_o1[["wGw"]] <- tcrossprod((DW %*% G), OW) %T% lag_double
  lags_o1 <- compact(lags_o1)
  if (!is.null(flow_indicator))
    lags_o1 <- lapply(lags_o1, "*", flow_indicator)

  lags_o2 <- named_list(c("wwG", "Gww", "wwGww", "wwGw", "wGww"))
  lags_o2[["wwG"]]   <- (DW %*% lags_o1[["wG"]]) %T% lag_left
  lags_o2[["Gww"]]   <- tcrossprod(lags_o1[["wG"]], OW) %T% lag_right
  lags_o2[["wwGww"]] <- tcrossprod((DW %*% lags_o1[["wGw"]]), OW) %T% lag_double
  lags_o2[["wwGw"]]  <- (DW %*% lags_o1[["wGw"]]) %T% lag_left
  lags_o2[["wGww"]]  <- tcrossprod(lags_o1[["wGw"]], OW) %T% lag_right
  if (!is.null(flow_indicator))
    lags_o2 <- lapply(lags_o2, "*", flow_indicator)


  lag_suffixes <- c("", paste0(".", c(names(lags_o1),names(lags_o2))))
  all_g <- c(named_list(name, G), lags_o1, lags_o2)
  names(all_g) <- paste0(name, lag_suffixes)
  return(all_g)
}




