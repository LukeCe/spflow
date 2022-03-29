#' @keywords internal
by_role_spatial_lags <- function(
  model_matrices,
  variable_roles,
  flow_control,
  flow_indicator,
  neighborhoods,
  mat_formatter){

  # 1. define required lags by data source
  sources <- c("pair", "orig", "dest")
  sources <- intersect(sources, names(model_matrices))
  sources <- lookup(sources)
  role_lookup <- sources_to_roles(!"dest" %in% sources)

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
  lag_requirements_by_source <- lapply(sources, "summarize_lags_by_source")

  # 2. compute lags for node data
  node_sources <- c("orig","dest")
  node_sources <- intersect(node_sources, names(model_matrices))
  role_prefixes <- list("D_" = "DEST_","O_" = "ORIG_","I_" = "INTRA_")

  lag_varnames_by_role <- lapply(lag_requirements_by_role, "suffix_sp_lags")
  lag_varnames_by_role <- lapply(lag_varnames_by_role, "unlist")

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

  if (isTRUE(flow_control[["twosls_decorrelate_instruments"]]))
    node_lags <- lapply(node_lags, "orthoginolize_instruments")

  # 3.) compute lags for pair data
  # ... Y_ (dependent variables)
  response_variables <- as.character(lag_requirements_by_role[["Y_"]])
  response_variables <- lookup(response_variables)

  mformat <- function(.v) mat_formatter(model_matrices$pair[,.v])
  flow_matrices <- lapply(response_variables, "mformat")
  flow_matrices <- Reduce("c", Map(
    f = "lag_flow_matrix",
    Y = flow_matrices,
    name = response_variables,
    MoreArgs = c(
      flow_control["model"],
      neighborhoods, # OW and DW
      list("flow_indicator" = flow_indicator))))

  # ... G_ (explanatory variables)
  pair_covariates <- unlist(lag_requirements_by_role[["G_"]])
  pair_covariates <- lookup(unique(pair_covariates))
  G_lag_num <- translist(lag_requirements_by_role[["G_"]])
  G_lag_num <- lapply(G_lag_num, function(x) length(x) - 1)
  keep_all_lags <- !isTRUE(flow_control[["twosls_reduce_pair_instruments"]])

  G_matrices <- lapply(pair_covariates, "mformat")
  G_matrices <- Reduce("c", lapply(pair_covariates,  function(.var) {
    double_lag_matrix(
      M = G_matrices[[.var]],
      DW = neighborhoods[["DW"]],
      OW = neighborhoods[["OW"]],
      name = .var,
      key = "G",
      flow_indicator = flow_indicator,
      lag_order = G_lag_num[[.var]],
      return_all_lags = keep_all_lags,
      lags_are_instruments = TRUE
    )}))

  return(c(list("Y_" = flow_matrices,
                "G_" = G_matrices),
           node_lags))

}

# ---- lag requirements -------------------------------------------------------

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


# ---- lag computations -------------------------------------------------------

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


#' @importFrom Matrix tcrossprod t
#' @keywords internal
double_lag_matrix <- function(
    M,
    DW,
    OW,
    name = "G",
    key = "G",
    flow_indicator = NULL,
    symmetric_lags = FALSE,
    lag_order = 2,
    return_all_lags = FALSE,
    lags_are_instruments = TRUE) {

  if (is.null(M))
    return(NULL)

  ### order 1 lag ###
  lags_0 <- named_list(name, M)
  if (lags_are_instruments)
    attr_inst_status(lags_0[[1]]) <- FALSE

  if ((is.null(OW) & is.null(DW)) | lag_order == 0)
    return(lags_0)

  ### lags of order 1 ###
  lags_1 <- named_list(c("w%s", "%sw", "w%sw"))

  # define controls
  left_lag <- !is.null(DW)
  right_lag <- !is.null(OW)
  double_lag <- left_lag & right_lag
  symmetric_lags <- symmetric_lags & double_lag
  return_all_lags <- return_all_lags | !double_lag

  wM <- (DW %*% M) %T% left_lag
  wMw <- tcrossprod(wM, OW) %T% double_lag
  # skip right lags if not needed
  Mw <- t(wM) %T% (symmetric_lags & right_lag & return_all_lags)
  Mw <- Mw %||% tcrossprod(M, OW) %T% (right_lag & return_all_lags)

  if (!is.null(flow_indicator)) {
    account_for_sparsity <- function(x) x * flow_indicator
    wMw <- wMw %|!|% account_for_sparsity
    Mw <- Mw %|!|% account_for_sparsity

    # skip sparsity if not returned
    if (lag_order >= 1)
      wM <- wM %|!|% account_for_sparsity
  }

  lags_1[["w%s"]]  <- wM %T% return_all_lags
  lags_1[["%sw"]]  <- Mw
  lags_1[["w%sw"]]  <- wMw
  names(lags_1) <- paste0(name, ".", sprintf(names(lags_1), key))

  if (lags_are_instruments)
    lags_1 <- lapply(lags_1, "attr_inst_status<-", TRUE)

  if (lag_order == 1)
    return(c(lags_0,lags_1))

  ### lags of order 2 ###
  lags_2 <- c("ww%s", "%sww", "ww%sw", "w%sww", "ww%sww")
  lags_2 <- named_list(lags_2)

  wwMw <- (DW %*% wMw) %T% double_lag
  wwMww <- tcrossprod(wwMw, OW) %T% double_lag
  # skip double left lag and all right lags if not needed
  wwM <- (DW %*% wM) %T% (left_lag & return_all_lags)
  Mww <- t(wwM) %T% (symmetric_lags & right_lag & return_all_lags)
  Mww <- Mww %||% tcrossprod(Mw, OW) %T% (right_lag & return_all_lags)
  wMww <- t(wwMw) %T% (symmetric_lags & double_lag  & return_all_lags)
  wMww <- wMww %||% (DW %*% Mww) %T% (double_lag  & return_all_lags)

  lags_2[["ww%s"]] <- wwM
  lags_2[["%sww"]] <- Mww
  lags_2[["ww%sw"]] <- wwMw %T% return_all_lags
  lags_2[["w%sww"]] <- wMww
  lags_2[["ww%sww"]] <- wwMww
  names(lags_2) <- paste0(name, ".", sprintf(names(lags_2), key))

  if (!is.null(flow_indicator))
    lags_2 <- lapply(lags_2, "account_for_sparsity")

  if (lags_are_instruments)
    lags_2 <- lapply(lags_2, "attr_inst_status<-", TRUE)

  return(c(lags_0,lags_1,lags_2))
}


# ---- instruments ------------------------------------------------------------
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

#' @keywords internal
`attr_inst_status<-` <- function(x, value) {
  attr(x, "is_instrument_var") <- value
  x
}

#' @keywords internal
attr_inst_status <- function(x) {
  attr(x, "is_instrument_var")
}

