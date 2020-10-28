#' Generate design matrices for the spatial interaction model
#'
#' Creates model matrices from a [sp_multi_network()] network.
#' For efficiency the relational representation of origin and destination data
#' is preserved.
#'
#' @inheritParams sp_network_pair
#' @inheritParams spflow
#'
#' @return A list of design matrices for spatial interaction model
spflow_model_matrix <- function(
  sp_multi_network,
  network_pair_id,
  flow_formula,
  flow_control
) {

  # identification of the data sources
  data_source_ids <- id(sp_multi_network)$network_pairs[[network_pair_id]]
  pair_id <- data_source_ids["pair"]
  orig_id <- data_source_ids["origin"]
  dest_id <- data_source_ids["destination"]

  # define which formulas will be carried to which data source
  pair_data_cases <- c("Y", "G")
  origin_data_cases <- c("OX")
  destination_data_cases <- c("DX")

  if (orig_id == dest_id) {
    IX <- NULL
    if (flow_control$use_intra) IX <- "IX"
    origin_data_cases <- c(destination_data_cases, origin_data_cases, IX)
    destination_data_cases <- NULL
  }

  neighborhoods <- neighborhoods(sp_multi_network, c(orig_id, dest_id))
  names(neighborhoods) <- c("OW","DW")

  formula_split <- interpret_flow_formula(
    flow_formula = flow_formula,
    flow_control = flow_control) %>%
    translist()

  # derive all matrices from the three possible data sources
  destination_model_matrix <- destination_data_cases %|!|%
    model_matrix_nodes(
      sp_network_nodes = network_nodes(sp_multi_network,dest_id),
      node_formulas = formula_split[destination_data_cases])

  origin_model_matrices <- origin_data_cases %|!|%
    model_matrix_nodes(
      sp_network_nodes = network_nodes(sp_multi_network, orig_id),
      node_formulas = formula_split[origin_data_cases])

  design_matrices_nodes <- c(origin_model_matrices,destination_model_matrix)

  # impose orthogonality of instruments from X
  is_relevant <- (flow_control$estimation_method == "s2sls" &&
                    flow_control$decorrelate_instruments)
  if (is_relevant) {
    design_matrices_nodes <- design_matrices_nodes %>%
      lapply(orthoginolize_instruments)

  }

  pair_model_matrices <-
    model_matrix_pairs(
      sp_network_pair = network_pairs(sp_multi_network,pair_id),
      pair_formulas = formula_split[pair_data_cases],
      orig_neighborhood = neighborhoods$OW,
      dest_neighborhood = neighborhoods$DW,
      flow_control = flow_control)

  # add information on constant terms
  # TODO rename extractor for network_nodes and network_paris to pull_...
  # TODO rename count to nnodes & npairs
  n_intra <- NULL
  if (flow_control$use_intra) {
    n_origins <- sp_multi_network %>%
      network_nodes(network_ids = orig_id) %>%
      count()
    n_intra <- n_origins
  }
  constants <- list(
    "const" = 1 %>% data.table::setattr(.,"is_instrument_var",FALSE),
    "const_intra" = n_intra %|!|%
      intra_regional_constant(
        W = neighborhoods$OW,
        use_instruments = flow_control$estimation_method == "s2sls"))

  return(c(constants,
           design_matrices_nodes,
           pair_model_matrices,
           neighborhoods))
}

model_matrix_nodes <- function(
  sp_network_nodes,
  node_formulas
) {

  ## create the global design matrix inlcuding all required spatial lags
  # ... add spatial lags according to the use of the variables...
  # ... information by role (sdm, normal ...)
  # ... instead of by data source (origin, destination, ...)
  nodes_design_matrix_global <- sp_nodes_design_matrix(
    sp_network_nodes,
    node_formulas %>% translist() %>% lapply(combine_formulas))

  ## re-arrange all variables by data source (origin, destination, ...)
  ## and declare the instrument statuts for each variable
  nodes_design_matrices <-
    split_by_source(nodes_design_matrix_global,
                    node_formulas,
                    dat_template(sp_network_nodes))

  return(nodes_design_matrices)
}

model_matrix_pairs <- function(
  sp_network_pair,
  pair_formulas,
  orig_neighborhood,
  dest_neighborhood,
  flow_control
) {

  # create the design matrix (in vector fromat)
  pair_design_matrix <- flow_model_frame(sp_network_pair,pair_formulas)
  n_o <-  count(sp_network_pair, "origins")
  n_d <-  count(sp_network_pair, "destinations")


  ## transform the flows ...
  # ...into matrix format and apply spatial lags
  response_variables <- extract_terms_labels(pair_formulas[["Y"]][["norm"]])
  flow_matrices <- pair_design_matrix %>%
    matrix_format(response_variables, n_d = n_d, n_o = n_o) %>%
    lapply(lag_flow_matrix,model = flow_control$model,
           OW = orig_neighborhood, DW = dest_neighborhood)

  # transform explanatory variables...
  # ... into matrix format
  explain_variables <- setdiff(colnames(pair_design_matrix),response_variables)
  explain_matrices <- pair_design_matrix %>%
    matrix_format(explain_variables, n_d = n_d, n_o = n_o)

  # ... apply two spatial lags to instruments zero to normal vars
  inst_formula <- pair_formulas[["G"]]$inst
  instruments <- inst_formula %|!|%
    extract_terms_labels(inst_formula,
                         pair_design_matrix[0,explain_variables,drop = FALSE])

  explain_lags <- 2 * (explain_variables %in% instruments)
  explain_matrices <- mapply(
    FUN = "apply_matrix_od_lags",
    G = explain_matrices, nb_lags = explain_lags, name = explain_variables,
    MoreArgs = list("OW" = orig_neighborhood, "DW" = dest_neighborhood),
    SIMPLIFY = FALSE
  )

  # ... declare instrument status
  non_inst_formula <- pair_formulas[["G"]]$norm
  non_instruments <-   non_inst_formula %|!|%
    extract_terms_labels(non_inst_formula,
                         pair_design_matrix[0,explain_variables,drop = FALSE])

  explain_matrices %>%
    lapply(function(.l) lapply(.l, set_instrument_status, TRUE)) %>%
    lapply(function(.l) {
      set_instrument_status(.l[[1]], !(names(.l)[1] %in% non_instruments))
      }) %>%
    invisible()

  return(list("Y" = flow_matrices %>% flatlist(),
              "G" = explain_matrices %>% flatlist()))
}

flow_model_frame <- function(object,case_formula){

  key_columns <- data.table::key(dat(object))
  combined_formula <- case_formula %>% flatlist() %>% combine_formulas()

  # validate that all information is available
  {
    available_vars <-
      variable_names(object) %>%
      c(".") %>%
      setdiff(key_columns)

    required_vars <- all.vars(combined_formula)
    unmatched_vars <- required_vars[!required_vars %in% available_vars]
    error_msg <-
      "The variables [%s] were not found in the data set associated to " %p%
      "the data describing the %s object with id [%s]!"

    assert(length(unmatched_vars) == 0,
           error_msg %>%
             sprintf(paste(unmatched_vars,collapse = " and "),
                     class(object),
                     id(object)))
  }

  flow_model_frame <-
    fix_contrast_model_matrix(
      formula = combined_formula,
      data    = dat(object)[,!key_columns, with = FALSE])

  return(flow_model_frame)

}

set_instrument_status <- function(x, is_inst) {
  data.table::setattr(x, "is_instrument_var", is_inst)
}

get_instrument_status <- function(x) {
  attr(x, "is_instrument_var")
}

# pair helpers ----
matrix_format <- function(mat_vec_fmt, columns, n_o, n_d){

  column_to_matrix <- function(col) {
    matrix(mat_vec_fmt[,col, drop = FALSE],
           nrow = n_o,
           ncol = n_d)
  }

  matrix_list <- columns %>% lapply(column_to_matrix)
  return(matrix_list)
}

lag_flow_matrix <- function(Y, model, OW, DW) {

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

  return(Y_lags)
}

apply_matrix_od_lags <- function(G, OW = NULL, DW = NULL, nb_lags = 0,
                                 name = "") {

  suffixes <- c("","lag" %p% seq_len(nb_lags))[seq_len(nb_lags + 1)]
  G_lags <- named_list(names = name %p% suffixes)
  G_lags[[1]] <- G

  # Default to identity
  OW <- OW %||% Matrix::Diagonal(nrow(G))
  DW <- DW %||% Matrix::Diagonal(ncol(G))

  for (i in seq_len(nb_lags)) {
    G_lags[[i + 1]] <- tcrossprod(OW %*% G_lags[[i]], DW)
  }

  return(G_lags)
}


## nodes helpers ----
required_sp_lags <- function(
  role_formulas,
  node_data_template) {

  # define which variable require which lags
  varnames_by_role <- role_formulas %>%
    lapply(function(.f) extract_terms_labels(.f,node_data_template)) %>%
    compact()

  # TODO instruments could also have 0 lags...
  vbr <- varnames_by_role
  required_lags <-
    list(
      "lag0" = vbr$norm,
      "lag1" = unique(c(vbr$sdm, vbr$inst)),
      "lag2" = vbr$inst,
      "lag3" = intersect(vbr$sdm,vbr$inst)
    ) %>% compact()

  return(required_lags)
}

sp_nodes_design_matrix <- function(
  sp_network_nodes,
  role_formulas) {

  # design matrix that includes tranformations but not lags
  node_design_matrix <- flow_model_frame(sp_network_nodes,role_formulas)

  # ... define then apply the required number of spatial lags
  # changes when the variable is sdm (+1) and when it is instrument (+2)
  lag_req <- required_sp_lags(
    role_formulas = role_formulas,
    node_data_template = dat_template(sp_network_nodes))

  lag_varnames <- suffix_sp_lags(lag_req) %>% flatten()
  W_nodes <- neighborhood(sp_network_nodes)

  # aplly all lags
  node_design_matrix <- rev(lag_req) %>%
    lapply(function(.ind) node_design_matrix[,.ind]) %>%
    reduce(function(.x1, .x2) { cbind(.x2, W_nodes %*% .x1) }) %>%
    as.matrix() %>%
    magrittr::set_colnames(., lag_varnames)

  return(node_design_matrix)
}

suffix_sp_lags <- function(lag_req) {
  suffix <- c(lag0 = "", lag1 = ".lag1", lag2 = ".lag2", lag3 = ".lag3")
  mapply(paste0, lag_req, suffix[names(lag_req)], SIMPLIFY = FALSE)
}


split_by_source <- function(global_design_matrix,
                            node_formulas,
                            node_data_template) {

  # define by data source which variable has which number of lags
  lags_by_source <-  node_formulas %>%
    lapply(required_sp_lags, node_data_template = node_data_template) %>%
    lapply(suffix_sp_lags)


  # extract the columns of the global design matrix and rename them
  prefixes <- c("IX" = "Intra_", "OX"  = "Orig_", "DX" = "Dest_")

  model_matrices <- lags_by_source %>%
    lapply(flatten) %>%
    compact() %>%
    lapply(function(.ind) global_design_matrix[,.ind, drop = FALSE]) %>%
    mapply(prefix_columns, obj = ., prefix = prefixes[names(.)],
           SIMPLIFY = FALSE)


  # get the number of "pure" instruments by model segment
  inst_by_source <- node_formulas %>%
    lapply(function(.l) { .l[["inst"]] %|!|%
        extract_terms_labels(.l[["inst"]], node_data_template)})

  non_inst_by_source <- node_formulas %>%
    lapply(function(.l) {
      non_inst_f <- .l[c("norm","sdm")] %>% compact()
      non_inst_f %|!|%
        combine_formulas(non_inst_f) %>%
        extract_terms_labels(node_data_template)})

  nb_inst_by_source <- mapply(function(i_vars, e_vars) {
    (2 * length(i_vars)) + 0 # sum(!i_vars %in% e_vars) ... once unlagged instr
  }, i_vars = inst_by_source, e_vars = non_inst_by_source)

  # convert count into logical according to column status
  mapply(function(.X , .nb_i) {
    inst_stat <- logical(ncol(.X))
    inst_stat[seq_len(.nb_i)] <- TRUE
    set_instrument_status(.X,rev(inst_stat))
    return(invisible(NULL))
    },
    .X = model_matrices, .nb_i = nb_inst_by_source[names(model_matrices)],
    SIMPLIFY = FALSE)

  return(model_matrices)
}

# other ----
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

intra_regional_constant <- function(W, use_instruments = FALSE) {

  In <- list(
    "In" =
      Matrix::Diagonal(nrow(W)) %>%
      data.table::setattr(name = "is_instrument_var", value = FALSE)
  )
  if (!use_instruments) {
    return(In)
  }

  V <- Matrix::tcrossprod(W) # def. V = WW'
  WV <- W %*% V
  WW <- W %*% W
  w_int <- list(
    "W" = W,
    "W'" = t(W),
    "WW" = WW,
    "WW'" = t(WW),
    "V" = V,
    "VV" = Matrix::tcrossprod(WV, W),
    "WV" = WV,
    "VW'" = t(WV)
  ) %>%
    lapply(data.table::setattr,
           name = "is_instrument_var",
           value = TRUE)

  return(c(In,w_int))
}

orthoginolize_instruments <- function(mat) {

  inst_index <- attr(mat,"is_instrument_var")
  no_instruments <- all(!inst_index)
  if (no_instruments)
    return(mat)

  vars <- mat[,!inst_index]
  inst_orth <- mat[,inst_index] %>%
    decorellate_matrix(cbind(1,vars)) %>%
    linear_dim_reduction(var_threshold = 0)

  new_matr <- cbind(vars,inst_orth)
  data.table::setattr(new_matr,name = "is_instrument_var",
                      value = inst_index[seq_len(ncol(new_matr))])

  return(new_matr)
}

