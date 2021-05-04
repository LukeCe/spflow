#' @keywords internal
by_role_spatial_lags <- function(
  model_matrices,
  variable_roles,
  neighborhoods,
  estim_control){

  ### 0) Define requirements
  lag_requirements_by_role <- lapply(variable_roles, "var_usage_to_lag")
  instrument_statu_by_role <- lapply(variable_roles, "var_usage_to_lag", TRUE)

  summarize_lags_by_source <- function(source_key){
    role_key <- role_lookup[[source_key]]
    result <- lapply(translist(lag_requirements_by_role[role_key]), "unlist")
    lapply(result, "unique")
  }
  sources <- intersect(c("pair", "orig", "dest"), names(model_matrices))
  role_lookup <- sources_to_roles(!"dest" %in% sources)
  lag_requirements_by_source <-
    lapply(lookup(sources), "summarize_lags_by_source")

  ### 1) Lag data for nodes
  node_sources <- intersect(c("orig","dest"), sources)
  lag_varnames_by_role <- lapply(lag_requirements_by_role, "suffix_sp_lags")
  lag_varnames_by_role <- lapply(lag_varnames_by_role, "unlist")

  apply_lags_to_node_source <- function(source_key) {
    role_keys <- role_lookup[[source_key]]
    nb_key <- c("orig" = "OW", "dest" = "DW")[source_key]
    source_nb <- neighborhoods[[nb_key]]
    source_mat <- model_matrices[[source_key]]
    lags <- lag_requirements_by_source[[source_key]]
    lags_names <- unlist(suffix_sp_lags(lags))

    # create one matrix for each source
    lagged_vars_mat <- as.matrix(Reduce(
      function(.x1, .x2) { cbind(.x2, source_nb %*% .x1) },
      lapply(rev(lags), function(.vars) cols_keep(source_mat,.vars))))
    colnames(lagged_vars_mat) <- lags_names

    # split the matrix by roles and declare instruments
    mat_by_role <- lapply(lag_varnames_by_role[role_keys],
                          function(.vars) cols_keep(lagged_vars_mat,.vars))

    inst_status <- lapply(instrument_statu_by_role[role_keys], "unlist")
    inst_status <- lapply(inst_status, "as.logical")

    Map("`attr_inst_status<-", x = mat_by_role, is_inst = inst_status)
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
    function(.col) estim_control$mat_format(model_matrices$pair[[,.col]]))

  flow_matrices <- Map(
    "lag_flow_matrix",
    Y = flow_matrices,
    name = response_variables,
    MoreArgs = list(
      model = estim_control[["model"]],
      OW = neighborhoods[["OW"]],
      DW = neighborhoods[["DW"]]))

  # ... G_ lags
  pair_covariates <-
    unique(unlist(lag_requirements_by_role[["G_"]], use.names = FALSE))
  covariate_matrices <- lapply(
    lookup(pair_covariates),
    function(.col) estim_control$mat_format(model_matrices$pair[[,.col]]))

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
    Map("attr_inst_status<-", x = G_lags,is_inst = inst)
  }
  lagged_covariate_matrices <- lapply(pair_covariates, "lag_pair_covariate")

  ### 3) Combine
  all_model_matrices <- c(
    list("Y_" = flatlist(drop_lnames(flow_matrices)),
         "G_" = flatlist(lagged_covariate_matrices)),
    flatlist(node_lags))

  return(all_model_matrices)
}

# ---- Helpers 1: defining requirements ---------------------------------------

#' @keywords internal
var_usage_to_lag <- function(.vars, out_inst = FALSE) {

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

  # variable order them into the required number of lags
  # optionally output the instrument status instead of the variable names
  inst_lookup <- function(.var,is_inst) {
    if (length(.var) == 0)
      return(NULL)

    if (out_inst)
      return(lookup(is_inst,.var))

    return(lookup(.var))
  }
  i <- function(.var) inst_lookup(.var,is_inst = TRUE)
  ni <- function(.var) inst_lookup(.var,is_inst = FALSE)

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
  plapply(lag_req, .f = paste0, suffix[names(lag_req)])
}

#' @keywords internal
set_instrument_status <- function(x, is_inst) {
  #TODO remove data.table
  data.table::setattr(x, "is_instrument_var", is_inst)
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

#' @keywords internal
get_instrument_status <- function(x) {
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
matrix_format <- function(mat_vec_fmt, columns, ...){

  args <- flatlist(list(...))
  column_to_matrix <- function(col) {
    do.call("vec_to_matrix",args = c(args, list(vec = mat_vec_fmt[,col])))
    }
  matrix_list <- lapply(lookup(columns), column_to_matrix)
  return(matrix_list)
}

#' @keywords internal
lag_flow_matrix <- function(Y, model, OW, DW, name = "Y") {

  names_rho <- define_spatial_lag_params(model)

  # destination case
  if (any(c("rho_d","rho_od","rho_odw") %in% names_rho)) {
    WY <- DW %*% Y
  }

  # origin case
  if (any(c("rho_o","rho_od","rho_odw") %in% names_rho)) {
    YW <- tcrossprod(Y,OW)
  }

  # orig-&-dest case
  if (any(c("rho_w","rho_odw") %in% names_rho)) {
    WYW <- OW %*% tcrossprod(Y,DW)
  }

  Y_lags <- switch(substr(model, 7, 7),   # (8.15) in LeSage book
                   "9" = list(Y, "d" = WY, "o" = YW, "w" = WYW),
                   "8" = list(Y, "d" = WY, "o" = YW, "w" = WYW),
                   "7" = list(Y, "d" = WY, "o" = YW),
                   "6" = list(Y, "odw" = (WY + YW + WYW)/3),
                   "5" = list(Y, "od"  = (WY + YW)/2),
                   "4" = list(Y, "w"   = WYW),
                   "3" = list(Y, "o"   = YW),
                   "2" = list(Y, "d"   = WY),
                   "1" = list(Y))


  names(Y_lags) <- name %p% c("",rep(".",length(names_rho))) %p% names(Y_lags)

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
apply_matrix_od_lags <- function(G, OW = NULL, DW = NULL,
                                 nb_lags = 0, name = "") {

  suffixes <- c("",".lag" %p% seq_len(nb_lags))[seq_len(nb_lags + 1)]
  G_lags <- named_list(names = name %p% suffixes)
  G_lags[[1]] <- G

  # Default to identity
  for (i in seq_len(nb_lags)) {
    G_lags[[i + 1]] <- sandwich_prod(OW,G_lags[[i]],DW)
  }

  return(G_lags)
}

#' @keywords internal
derive_pair_instruments <- function(
  G,
  OW,
  DW,
  name = "G",
  full_inst = FALSE) {

  if (is.null(G))
    return(NULL)

  if (is.null(OW) & is.null(DW))
    return(named_list(name,G))

  if (is.null(OW) | is.null(DW))
    full_inst <- TRUE

  # initialize lags to null
  # then compute required ones
  g_lags <- c("wG","wwG","Gw","Gww","wGw","wwGw","wGww","wwGww")
  lapply(g_lags, "assign", value = NULL)

  # destination lags
  d <- TRUE %T% (!is.null(DW))
  wG <- d %|!|% (DW %*% G)
  wwG <- d %|!|% (DW %*% wG)

  # origin lags
  # (not needed when working with reduced instruments and d is given)
  o <- (TRUE %T% !is.null(OW)) %T% full_inst
  Gw <- o %|!|% tcrossprod(G, OW)
  Gww <- o %|!|% tcrossprod(Gw, OW)

  # o-d lags
  do <- TRUE %T% (!is.null(OW) & !is.null(DW))
  wGw <- do %|!|% tcrossprod(wG,OW)
  wwGw <- do %|!|% tcrossprod(wwG,OW)
  wGww <- do %|!|% ((DW %*% Gww) %T% o)
  wwGww <- do %|!|% tcrossprod(wwGw, OW)

  # remove super fluent instruments in the reduced case
  if (!full_inst & isTRUE(do)) {
    wG <- wwG <- wwGw <- NULL
  }

  # collect and name matrices
  G_obj <- c("G",g_lags)
  lag_name <- lookup(names = G_obj,
                     values = name %p% c("", ".lag." %p% g_lags))
  G_inst <- compact(collect(c("G",g_lags)))
  names(G_inst) <- lag_name[names(G_inst)]
  return(G_inst)
}




