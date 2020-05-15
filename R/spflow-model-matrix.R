#' Genrate design matrices for the spatial interaction model
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
  pair_data_cases <- c("pair_", "interactions")
  origin_data_cases <- c("orig_")
  destination_data_cases <- c("dest_")

  if (orig_id == dest_id) {
    origin_data_cases <- c(destination_data_cases, origin_data_cases, "intra_")
    destination_data_cases <- NULL
  }

  flow_formulas_by_cases <- model_formula_decompose(
    flow_formula = flow_formula,
    flow_control = flow_control) %>%
    translist()

  # derive all matrices from the three possible datasources
  destination_model_matrix <-
    destination_data_cases %|!|%
    model_matrix_expand_net(
      case_formulas = flow_formulas_by_cases[destination_data_cases],
      case_data = dat(multi_net_examples, dest_id),
      case_neighborhoods = neighborhoods(multi_net_examples, dest_id))

  origin_model_matrices <-
    model_matrix_expand_net(
      case_formulas = flow_formulas_by_cases[origin_data_cases],
      case_data = dat(multi_net_examples,orig_id),
      case_neighborhoods = neighborhoods(multi_net_examples,orig_id))

  pair_model_matrices <-
    model_matrix_expand_pairs(
      pair_formulas = flow_formulas_by_cases[pair_data_cases],
      network_pair = network_pairs(sp_multi_network,pair_id),
      pair_neighborhoods =
        neighborhoods(sp_multi_network, c(orig_id ,dest_id)),
      flow_control = flow_control)

  return(c(destination_model_matrix,
           origin_model_matrices,
           pair_model_matrices))
}

model_matrix_expand_net <- function(
  case_formulas,
  case_data,
  case_neighborhoods
  ) {

  # derive the global model matrix by combining
  # all formulas for the same datasource
  key_clomuns <- data.table::key(case_data)

  model_matrix_global <-
    fix_contrast_model_matrix(
      formula = flatlist(case_formulas) %>% combine_formulas(),
      data    = case_data[,!key_clomuns, with = FALSE])

  ### add spatial lags according to the use of the variables
  formulas_by_lag <- case_formulas %>%
    translist() %>%
    lapply(combine_formulas)

  # define which variable require which lags
  lag_which_vars <- function(.form) {
    extract_terms_labels(
      formula = formulas_by_lag$normal_variables,
      fake_data = case_data[0,!key_clomuns, with = FALSE])
  }

  varnames_by_role <- formulas_by_lag %>%
    lapply(lag_which_vars)

  lag_requirements <- define_spatial_lag_requirements(varnames_by_role)

  # add spatial lags to the model matrix
  left_lag_and_cbind <- function(.x1, .x2) {
    cbind(.x2,case_neighborhoods[[1]] %*% .x1)
  }

  model_matrix_global <- lag_requirements %>%
    rev() %>%
    lapply(function(.ind) model_matrix_global[,.ind]) %>%
    reduce(left_lag_and_cbind)

  # split the matrix by variable type
  add_lag_suffixes <- function(lag_req) {
    mapply(paste0, lag_req, c("","_lag1","_lag2","_lag3"))
  }

  colnames(model_matrix_global) <- add_lag_suffixes(lag_requirements)

  lag_types_model_segments <-
    case_formulas %>%
    lapply(lapply, lag_which_vars)

  lag_requirements_by_model_segments <-
    lag_types_model_segments %>%
    lapply(define_spatial_lag_requirements) %>%
    lapply(add_lag_suffixes)

  segment_model_matrices <-
    lag_requirements_by_model_segments %>%
    lapply(function(.ind) model_matrix_global[,.ind])

  ### add information on instruments
  is_instrument_attribute <- function(dat,num_inst) {
    n_var <- ncol(dat)
    is_instrument <- vector("logical", n_var)
    is_instrument[seq_len(num_inst)] <- TRUE
    rev(is_instrument)
  }

  # information on the number of instruments
  instruments_by_model_segment <-
    lag_types_model_segments %>%
    lapply(function(.l) length(.l["instrumental_variables"]) * 2) %>%
    mapply(is_instrument_attribute,
           dat = segment_model_matrices,
           num_inst = .,
           SIMPLIFY = FALSE)

  # set attribute which allows easy subsetting
  mapply(function(.dat,.inst) {
    data.table::setattr(.dat, "instrument_column",.inst)},
         .dat = segment_model_matrices,
         .inst = instruments_by_model_segment,
         SIMPLIFY = FALSE) %>%
    invisible()

  math_names <- c("dest_" = "DX",  "orig_" = "OX",  "intra_" = "IX")
  names(segment_model_matrices) <- math_names[names(segment_model_matrices)]

  return(segment_model_matrices)
}

model_matrix_expand_pairs <- function(
  pair_formulas,
  network_pair,
  pair_neighborhoods,
  flow_control
) {

  # derive the global model matrix by combining
  # all formulas for the same datasource
  key_clomuns <- data.table::key(dat(network_pair))

  model_matrix_global <-
    fix_contrast_model_matrix(
      formula = flatlist(pair_formulas) %>% combine_formulas(),
      data    = dat(network_pair)[,!key_clomuns, with = FALSE])

  # split into response and exogenouse variables
  # convert to matrices for efficiency
  data_template <- dat(network_pair)[0,!key_clomuns, with = FALSE]

  case_variables <-
    pair_formulas %>%
    lapply(combine_formulas) %>%
    lapply(extract_terms_labels,
           fake_data = data_template) %>%
    lapply(list_lookup)

  n_orig <- count(network_pair, "origins")
  n_dest <- count(network_pair, "destinations")

  column_to_matrix <- function(col) {
    Matrix::Matrix(model_matrix_global[,col],
                   nrow = n_orig,
                   ncol = n_dest)
  }

  response_matrices <-
    lapply(case_variables$interactions, column_to_matrix)

  explanatory_matrices <-
    lapply(case_variables$pair_, column_to_matrix)

  ## determine the number of lags ...
  # ... for the response variables
  # ... the number of lags is driven by the spatial auto-regressive parameters
  names_rho <- identify_auto_regressive_parameters(flow_control$model)

  response_matrices <- response_matrices %>%
    lapply("compute_lagged_interactions",
           names_rho,
           flow_control$model,
           OW = pair_neighborhoods[[1]],
           DW = pair_neighborhoods[[2]])

  # ... for exogenous variables
  # ... the number of lags depends on the instrument status
  pair_instruments <- extract_matrix_vars(
    pair_formulas$pair_$instrumental_variables,
    data_template)

  lag_number <- 2
  pair_lags <- case_variables$pair_ %>%
    lapply(function(.var) (.var %in% pair_instruments)*lag_number)

  lag_instruments <- function(.G,.num_lags) {
    apply_matrix_od_lags(G = .G,
                         OW = pair_neighborhoods[[1]],
                         DW = pair_neighborhoods[[2]],
                         nb_lags = .num_lags)
  }

  explanatory_matrices <- mapply(
    "lag_instruments",
    .G = explanatory_matrices,
    .num_lags = pair_lags,
    SIMPLIFY = FALSE
    )

  # add information on instrument status
  # all are instruments
  explanatory_matrices %>%
    lapply(function(.l)
      lapply(.l,data.table::setattr,"instrument_column",TRUE)) %>%
    invisible()

  # except the first elements
  explanatory_matrices %>%
    lapply(function(.l)
      data.table::setattr(.l[[1]], "instrument_column",FALSE))

  return(list("Y" = response_matrices %>% flatten(),
              "G" = explanatory_matrices %>% flatten()))
}

define_spatial_lag_requirements <- function(
  formulas_by_lag_type) {

  required_lags <-  with(
    formulas_by_lag_type,{
      list(
        "lag0" = normal_variables,
        "lag1" = unique(c(sdm_variables, instrumental_variables)),
        "lag2" = instrumental_variables,
        "lag3" = intersect(sdm_variables,instrumental_variables)
      )})

  return(required_lags)

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

compute_lagged_interactions <- function(
  Y,
  names_rho,
  model,
  OW,
  DW) {

  # destination case
  if (any(c("rho_d","rho_od","rho_odw") %in% names_rho)) {
    WY <- OW %*% Y
  }
  # origin case
  if (any(c("rho_o","rho_od","rho_odw") %in% names_rho)) {
    YW <- tcrossprod(Y,DW)
  }
  # orig-&-dest case
  if (any(c("rho_w","rho_odw") %in% names_rho)) {
    WYW <- OW %*% tcrossprod(Y,DW)
  }

  Y_lags <- switch(substr(model, 7, 7),   # (8.15) in LeSage book
                   "9" = list(Y, "o" = WY, "d" = YW, "w" = WYW),
                   "8" = list(Y, "o" = WY, "d" = YW, "w" = WYW),
                   "7" = list(Y, "o" = WY, "d" = YW, "w" = WYW),
                   "6" = list(Y, "odw" = (WY + YW + WYW)/3),
                   "5" = list(Y, "od"  = (WY + YW)/2),
                   "4" = list(Y, "w"   = WYW),
                   "3" = list(Y, "d"   = YW),
                   "2" = list(Y, "o"   = WY),
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
