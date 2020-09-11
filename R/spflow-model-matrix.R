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

  flow_formulas_by_cases <- interpret_flow_formula(
    flow_formula = flow_formula,
    flow_control = flow_control) %>%
    translist()

  # derive all matrices from the three possible data sources
  ## TODO assert that all variables are given
  destination_model_matrix <-
    destination_data_cases %|!|%
    model_matrix_expand_net(
      case_formulas = flow_formulas_by_cases[destination_data_cases],
      case_data = dat(sp_multi_network, dest_id),
      case_neighborhood = neighborhoods$DW)

  origin_model_matrices <-
    model_matrix_expand_net(
      case_formulas = flow_formulas_by_cases[origin_data_cases],
      case_data = dat(sp_multi_network,orig_id),
      case_neighborhood = neighborhoods$OW)

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
      pair_formulas = flow_formulas_by_cases[pair_data_cases],
      orig_neighborhood = neighborhoods$OW,
      dest_neighborhood = neighborhoods$DW,
      flow_control = flow_control)

  # add information on constant terms
  n_intra <- nrow(origin_model_matrices$IX)
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

  # create the design matrix
  node_design_matrix <- flow_model_frame(sp_network_nodes,node_formulas)


  # ... define then apply the required number of spatial lags
  # changes when the variable is sdm (+1) and when it is instrument (+2)

  # TODO infish node design matrix
  stop("Not implemented")
  lag_node_data <- required_sp_lags(
    node_formulas = node_formulas,
    node_data_template = dat(sp_network_nodes)[])




}

model_matrix_expand_net <- function(
  case_formulas,
  case_data,
  case_neighborhood
  ) {

  # derive the global model matrix by combining
  # all formulas for the same data source
  key_columns <- data.table::key(case_data)

  model_matrix_global <-
    fix_contrast_model_matrix(
      formula = flatlist(case_formulas) %>% combine_formulas(),
      data    = case_data[,!key_columns, with = FALSE])

  ### add spatial lags according to the use of the variables
  formulas_by_lag <- case_formulas %>%
    translist() %>%
    lapply(combine_formulas)

  # define which variable require which lags
  lag_which_vars <- function(.form) {
    extract_terms_labels(
      formula = .form,
      fake_data = case_data[0,!key_columns, with = FALSE])
  }

  varnames_by_role <- formulas_by_lag %>%
    lapply(lag_which_vars) %>%
    compact()

  lag_requirements <- define_spatial_lag_requirements(varnames_by_role)

  # add spatial lags to the model matrix
  left_lag_and_cbind <- function(.x1, .x2) {
    cbind(.x2,case_neighborhood %*% .x1)
  }

  model_matrix_global <- lag_requirements %>%
    rev() %>%
    lapply(function(.ind) model_matrix_global[,.ind]) %>%
    reduce(left_lag_and_cbind) %>%
    as.matrix()

  # split the matrix by variable type
  add_lag_suffixes <- function(lag_req) {
    lag_to_suffix <-  c(lag0 = ""     , lag1 = ".lag1",
                        lag2 = ".lag2", lag3 = ".lag3")

    map2(lag_req, lag_to_suffix[names(lag_req)],paste0) %>%
      flatten()
  }

  colnames(model_matrix_global) <- add_lag_suffixes(lag_requirements)

  lag_types_model_segments <-
    case_formulas %>%
    lapply(lapply, lag_which_vars)

  lag_requirements_by_model_segments <-
    lag_types_model_segments %>%
    lapply(define_spatial_lag_requirements) %>%
    lapply(add_lag_suffixes)

  role_prefixes <-
    c("IX" = "Intra_",
      "OX"  = "Orig_",
      "DX" = "Dest_")

  segment_model_matrices <-
    lag_requirements_by_model_segments %>%
    compact() %>%
    lapply(function(.ind) model_matrix_global[,.ind]) %>%
    mapply(prefix_columns,
           obj = .,
           prefix = role_prefixes[names(.)],
           SIMPLIFY = FALSE)


  ### add information on instruments
  is_instrument_attribute <- function(dat,num_inst) {
    n_var <- ncol(dat)
    is_instrument <- vector("logical", n_var)
    is_instrument[seq_len(num_inst)] <- TRUE
    rev(is_instrument)
  }

  # information on the number of instruments
  instrument_key <- "inst"
  nb_inst <- 2
  instruments_by_model_segment <-
    lag_types_model_segments %>%
    lapply(function(.l) length(.l[[instrument_key]]) * nb_inst) %>%
    mapply(is_instrument_attribute,
           dat = segment_model_matrices,
           num_inst = .,
           SIMPLIFY = FALSE)

  # set attribute which allows easy subsetting
  mapply(function(.dat,.inst) {
    data.table::setattr(.dat, "is_instrument_var",.inst)},
         .dat = segment_model_matrices,
         .inst = instruments_by_model_segment,
         SIMPLIFY = FALSE) %>%
    invisible()

  return(segment_model_matrices)
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

set_instrument_status <- function(x, is_inst) {
    data.table::setattr(x, "is_instrument_var", is_inst)
}

required_sp_lags <- function(
  node_formulas,
  node_data_template) {

  # add spatial lags according to the use of the variables...
  # ... information by role (sdm, normal ...)
  # ... instead of by data source (origin, destination, ...)
  formulas_by_lag <- node_formulas %>%
    translist() %>%
    lapply(combine_formulas)

  # define which variable require which lags
  varnames_by_role <- formulas_by_lag %>%
    lapply(function(.f) extract_terms_labels(.f,node_data_template)) %>%
    compact()

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

define_spatial_lag_requirements <- function(
  formulas_by_lag_type) {

  normal_vars <- formulas_by_lag_type$norm
  sdm_vars <- formulas_by_lag_type$sdm
  instrument_vars <- formulas_by_lag_type$inst

  required_lags <-
      list(
        "lag0" = normal_vars,
        "lag1" = unique(c(sdm_vars, instrument_vars)),
        "lag2" = instrument_vars,
        "lag3" = intersect(sdm_vars,instrument_vars)
      ) %>% compact()

  return(required_lags)

}

define_matrix_keys <- function() {
  c("origin_neighborhood"       = "OW",
    "destination_neighborhood" = "DW",
    "constant"                 = "const",
    "intra_constant"           = "const_intra",
    "dest_"                    = "DX",
    "orig_"                    = "OX",
    "intra_"                   = "IX",
    "pair_"                    = "G",
    "interactions"             = "Y")
}

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

