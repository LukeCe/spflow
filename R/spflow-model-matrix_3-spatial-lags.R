#' @keywords internal
by_role_spatial_lags <- function(
  source_model_matrices,
  lag_requirements,
  neighborhoods,
  matrix_form_arguments,
  model,
  decorrelate_instruments = FALSE,
  reduce_pair_instruments = TRUE){

  # define lag requirements by role and summarize them by source
  role_var_lags <- lapply(lag_requirements, "var_usage_to_lag")
  inst_status_lags <- lapply(lag_requirements, "var_usage_to_lag", TRUE)
  sources <- c("pair","orig","dest")
  sources <- sources %>% intersect(names(source_model_matrices))

  is_within_flow <- !"dest" %in% sources
  role_lookup <- sources_to_roles(is_within_flow)
  summarize_lags_by_source <- function(source_key){
    role_key <- role_lookup[[source_key]]
    role_var_lags[role_key] %>%
      translist() %>%
      lapply(unlist) %>%
      lapply(unique)
  }
  source_var_lags <- lookup(sources) %>% lapply("summarize_lags_by_source")

  ### 1) node data lags
  node_sources <- c("orig","dest")
  node_sources <- node_sources %>% intersect(sources)
  role_var_lag_names <- role_var_lags %>%
    lapply(suffix_sp_lags) %>%
    lapply(unlist)

  apply_lags_to_node_source <- function(source_key) {
    role_keys <- role_lookup[[source_key]]
    nb_key <- c("orig" = "OW", "dest" = "DW")[source_key]
    source_nb <- neighborhoods[[nb_key]]
    source_mat <- source_model_matrices[[source_key]]
    lags <- source_var_lags[[source_key]]
    lags_names <- suffix_sp_lags(lags) %>% flatten()

    # create one matrix for each source
    lagged_vars_mat <-
      lapply(rev(lags), function(.vars) cols_keep(source_mat,.vars)) %>%
      lreduce(function(.x1, .x2) { cbind(.x2, source_nb %*% .x1) }) %>%
      as.matrix() %>%
      set_col_names(., lags_names)

    # split the matrix by roles and declare instruments
    mat_by_role <- role_var_lag_names[role_keys] %>%
      lapply(function(.vars) cols_keep(lagged_vars_mat,.vars))
    inst_status <- inst_status_lags[role_keys] %>%
      lapply(unlist) %>% lapply(as.logical)

    plapply(x = mat_by_role, is_inst = inst_status,
            .f = "set_instrument_status")
    mat_by_role
  }

  node_lags <- lapply(node_sources, "apply_lags_to_node_source") %>%
    flatlist()

  # impose orthogonality of instruments from X
  if (decorrelate_instruments) {
    node_lags <- node_lags %>% lapply(orthoginolize_instruments)
  }

  ### 2) pair data: generate, then split lags
  # ... Y_ lags
  response_variables <- role_var_lags$Y_ %>% flatten(use.names = FALSE)
  flow_matrices <- source_model_matrices$pair %>%
    matrix_format(response_variables, matrix_form_arguments) %>%
    plapply(Y = ., name = response_variables,
            .f = "lag_flow_matrix",
            fix_args = list(model = model,
                            OW = neighborhoods$OW,
                            DW = neighborhoods$DW))

  # ... G_ lags
  pair_covariates <- role_var_lags$G_ %>%
    flatten(use.names = FALSE) %>% unique()
  covariate_matrices <- source_model_matrices$pair %>%
    matrix_format(pair_covariates, matrix_form_arguments)
  lag_pair_covariate <- function(var){
    var_lags <- role_var_lags$G_ %>% lfilter(function(x) x == var) %>% length()
    G_lags <- derive_pair_instruments(
      G = covariate_matrices[[var]],
      OW = neighborhoods$OW,
      DW = neighborhoods$DW,
      name = var,
      full_inst = !isTRUE(reduce_pair_instruments)
    ) %T% (var_lags > 1) %||% covariate_matrices[var]

    inst_a <- inst_status_lags$G_ %>% lapply("[", var) %>% unlist()
    inst <- !logical(length(G_lags))
    inst[seq_along(inst_a)] <- inst_a
    plapply(x = G_lags,is_inst = inst,.f = "set_instrument_status")
    G_lags
  }
  lagged_covariate_matrices <- lapply(pair_covariates, "lag_pair_covariate")

  # combine
  all_model_matrices <- c(
    list("Y_" = flow_matrices %>% drop_lnames() %>% flatlist(),
         "G_" = lagged_covariate_matrices %>% flatlist()),
    node_lags %>% flatlist())

  return(all_model_matrices)
}

#' @keywords internal
var_usage_to_lag <- function(.vars, out_inst = FALSE) {

  # pull out variables and declare their instrument status
  norm <- .vars$norm
  sdm <- .vars$sdm
  inst <- .vars$inst

  inst0 <- inst %>% setdiff(norm) %>% setdiff(sdm)
  inst1 <- inst %>% setdiff(sdm)
  inst2 <- inst %>% setdiff(inst0)
  inst3 <- inst %>% setdiff(inst0) %>% intersect(sdm)

  # variable order them into the required number of lags
  # optionally output the instrument status instead of the variable names
  inst_lookup <- function(.var,is_inst) {
    if (length(.var) == 0)
      return(NULL)

    if (!out_inst)
      return(lookup(.var))

    lookup(is_inst,.var)
  }
  i <- function(.var) inst_lookup(.var,is_inst = TRUE)
  ni <- function(.var) inst_lookup(.var,is_inst = FALSE)

  required_lags <-
    list(
      "lag0" = c(ni(norm), i(inst0)),
      "lag1" = c(ni(sdm),i(inst1)),
      "lag2" = i(inst2),
      "lag3" = i(inst3)
    ) %>% compact() %>% lapply("sort_names")

  return(required_lags)

}

#' @keywords  internal
sources_to_roles <- function(is_within) {
  D_ <- "D_" %T% is_within
  list("pair" = c("Y_", "G_"),
       "orig" = c("O_", D_, "I_"),
       "dest" = "D_" %T% (!is_within)) %>% compact()
}

#' @keywords internal
suffix_sp_lags <- function(lag_req) {
  suffix <- c(lag0 = "", lag1 = ".lag1", lag2 = ".lag2", lag3 = ".lag3")
  plapply(lag_req, .f = paste0, suffix[names(lag_req)])
}

#' @keywords internal
set_instrument_status <- function(x, is_inst) {
  data.table::setattr(x, "is_instrument_var", is_inst)
}

#' @keywords internal
get_instrument_status <- function(x) {
  attr(x, "is_instrument_var")
}

#' @keywords internal
orthoginolize_instruments <- function(mat) {

  inst_index <- get_instrument_status(mat)
  no_instruments <- none(inst_index)
  if (no_instruments)
    return(mat)

  vars <- mat[,!inst_index]
  inst_orth <- mat[,inst_index] %>%
    decorellate_matrix(cbind(1,vars)) %>%
    linear_dim_reduction(var_threshold = 1e-4)

  new_matr <- cbind(vars,inst_orth)
  set_instrument_status(new_matr, inst_index[seq_len(ncol(new_matr))])

  return(new_matr)
}

#' @keywords internal
matrix_format <- function(mat_vec_fmt, columns, ...){

  args <- list(...) %>% flatlist()
  column_to_matrix <- function(col) {
    do.call("vec_to_matrix",args = c(args, list(vec = mat_vec_fmt[,col])))
    }
  matrix_list <- lookup(columns) %>% lapply(column_to_matrix)
  return(matrix_list)
}

#' @keywords internal
lag_flow_matrix <- function(Y, model, OW, DW, name = "Y") {

  names_rho <- identify_auto_regressive_parameters(model)

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
identify_auto_regressive_parameters <- function(model) {
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
derive_pair_instruments <- function(G,OW,DW,name = "G", full_inst = FALSE) {


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
  d <- T %T% (!is.null(DW))
  wG <- d %|!|% (DW %*% G)
  wwG <- d %|!|% (DW %*% wG)

  # origin lags
  # (not needed when working with reduced instruments and d is given)
  o <- (T %T% !is.null(OW)) %T% full_inst
  Gw <- o %|!|% tcrossprod(G, OW)
  Gww <- o %|!|% tcrossprod(Gw, OW)

  # o-d lags
  do <- T %T% (!is.null(OW) & !is.null(DW))
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
  lag_name <- lookup(names = G_obj,values = name %p% c("", ".lag." %p% g_lags))
  G_inst <- collect(c("G",g_lags)) %>% compact() %>%
    set_lnames(lag_name[names(.)])
  return(G_inst)
}




