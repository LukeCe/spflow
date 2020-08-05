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
  pair_id <- data_source_ids["network_pair_id"]
  orig_id <- data_source_ids["origin_network_id"]
  dest_id <- data_source_ids["destination_network_id"]

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

  # impose orthogonality of instruments from X
  design_matrices_nodes <-
    c(origin_model_matrices,destination_model_matrix) %>%
    lapply(orthoginolize_instruments)


  pair_model_matrices <-
    model_matrix_expand_pairs(
      pair_formulas = flow_formulas_by_cases[pair_data_cases],
      network_pair = network_pairs(sp_multi_network,pair_id),
      pair_neighborhoods = neighborhoods,
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

model_matrix_expand_pairs <- function(
  pair_formulas,
  network_pair,
  pair_neighborhoods,
  flow_control
) {

  # derive the global model matrix by combining
  # all formulas for the same data source
  key_columns <- data.table::key(dat(network_pair))

  model_matrix_global <-
    fix_contrast_model_matrix(
      formula = flatlist(pair_formulas) %>% combine_formulas(),
      data    = dat(network_pair)[,!key_columns, with = FALSE])

  n_orig <- count(network_pair, "origins")
  n_dest <- count(network_pair, "destinations")
  column_to_matrix <- function(col) {
    matrix(model_matrix_global[,col],
           nrow = n_orig,
           ncol = n_dest)
  }

  ## create the lagged interactions Y: response variable
  response_formula <- pair_formulas[[1]][[1]]
  response_variables <- extract_terms_labels(response_formula)
  response_matrices <-
    response_variables %>%
    lapply(column_to_matrix) %>%
    lapply(lag_flow_matrix,
           model = flow_control$model,
           OW = pair_neighborhoods$OW,
           DW = pair_neighborhoods$DW)

  ## create the matrix form of pair attributes G: explanatory variables
  # we have to ensure that the response variable does not enter these matrices
  explain_formulas <- pair_formulas[[2]]
  explain_vars <- colnames(model_matrix_global) %>% setdiff(response_variables)
  explain_matrices <- lapply(explain_vars, column_to_matrix)

  # lag those explanatory variables that are used as instruments
  explain_data_template <-
    dat(network_pair)[0,!c(key_columns,response_variables), with = FALSE]
  explain_instruments <- explain_formulas$inst %|!|%
    extract_terms_labels(explain_formulas$inst,explain_data_template)

  nb_lags <- 2
  lag_explain_vars <- integer(length(explain_vars))
  lag_explain_vars[explain_vars %in% explain_instruments] <- nb_lags
  lag_pair_instruments <- function(.G,.num_lags,.name) {
    apply_matrix_od_lags(G = .G,
                         OW = pair_neighborhoods[[1]],
                         DW = pair_neighborhoods[[2]],
                         nb_lags = .num_lags,
                         name = .name)
  }

  explain_matrices <- mapply(
    lag_pair_instruments,
    .G = explain_matrices,
    .num_lags = lag_explain_vars,
    .name = explain_vars,
    SIMPLIFY = FALSE
  )

  # add information on instrument status
  # ... all are instruments except the first elements
  set_inst <- function(x, is_inst) {
    data.table::setattr(x, "is_instrument_var", is_inst)
  }

  explain_matrices %>%
    lapply(function(.l) lapply(.l, set_inst, TRUE)) %>%
    lapply(function(.l) set_inst(.l[[1]], FALSE)) %>%
    invisible()

  return(list("Y" = response_matrices %>% flatlist(),
              "G" = explain_matrices %>% flatlist()))
}

lag_flow_matrix <- function(
  Y,
  model,
  OW,
  DW) {

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
                   "7" = list(Y, "d" = WY, "o" = YW, "w" = WYW),
                   "6" = list(Y, "odw" = (WY + YW + WYW)/3),
                   "5" = list(Y, "od"  = (WY + YW)/2),
                   "4" = list(Y, "w"   = WYW),
                   "3" = list(Y, "o"   = YW),
                   "2" = list(Y, "d"   = WY),
                   "1" = list(Y))

  return(Y_lags)
}


apply_matrix_od_lags <- function(
  G,
  OW = NULL,
  DW = NULL,
  nb_lags = 0,
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
  vars <- mat[,!inst_index]
  inst_orth <- mat[,inst_index] %>%
    decorellate_matrix(cbind(1,vars)) %>%
    linear_dim_reduction(inst_orth,var_threshold = 0.7)

  new_matr <- cbind(vars,inst_orth)
  data.table::setattr(new_matr,name = "is_instrument_var",
                      value = inst_index[seq_len(ncol(new_matr))])

  return(cbind(vars,inst_orth))
}

