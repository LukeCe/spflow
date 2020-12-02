#' Generate design matrices for the spatial interaction model
#'
#' Creates model matrices from a [sp_multi_network()] network.
#' For efficiency the relational representation of origin and destination data
#' is preserved.
#'
#' @inheritParams sp_network_pair
#' @inheritParams spflow
#'
#' @keywords internal
#' @return A list of design matrices for spatial interaction model
spflow_model_matrix <- function(
  sp_multi_network,
  network_pair_id,
  flow_formula,
  flow_control
) {

  formula_parts <- interpret_flow_formula(flow_formula, flow_control)

  # transform original variables
  data_sources <- pull_flow_data(sp_multi_network, network_pair_id)
  model_matrices <- by_source_model_matrix(formula_parts, data_sources)

  # prepare arguments for creation of spatial lags and matrix forms
  lag_requirements <- def_spatial_lag_requirements(formula_parts, data_sources)
  matrix_infos <- def_matrix_form_args(sp_multi_network, network_pair_id)
  neighborhoods <- pull_neighborhood_data(sp_multi_network, network_pair_id)

  # generate the spatial lags by source then split by role
  model_matrices <- by_role_spatial_lags(
    source_model_matrices = model_matrices,
    lag_requirements = lag_requirements,
    neighborhoods = neighborhoods,
    matrix_form_arguments = matrix_infos,
    model = flow_control$model)

  # Extract weights and constants if they are defined
  constants <- define_flow_constants(
    const_formula = formula_parts$const,
    use_instruments = flow_control$estimation_method == "s2sls",
    OW = neighborhoods$OW)

  weights <- define_flow_weights(data_sources$pair,
                                 flow_control$weight_var,
                                 matrix_infos)

  return(c(model_matrices, neighborhoods,
           list("constants" = constants, "weights" = weights)))
}


#' @importFrom data.table key copy
#' @keywords internal
pull_flow_data <- function(sp_multi_network, network_pair_id) {

  # identification of the data sources
  data_source_ids <- id(sp_multi_network)$network_pairs[[network_pair_id]]
  pair_id <- data_source_ids["pair"]
  orig_id <- data_source_ids["orig"]
  dest_id <- data_source_ids["dest"]

  orig_data <- pull_nodes(sp_multi_network,orig_id)
  dest_data <- pull_nodes(sp_multi_network,dest_id) %T% (orig_id != dest_id)
  pair_data <- pull_pairs(sp_multi_network,pair_id)

  flow_data <- list("orig" = orig_data,
                    "dest" = dest_data,
                    "pair" = pair_data) %>% compact() %>%
    lapply("dat") %>% copy()

  # define completeness and extract the keys
  flow_data <- flow_data %>% lapply(function(.d) cols_drop(.d,key(.d)))

  return(flow_data)
}

#' @keywords internal
pull_neighborhood_data <-  function(sp_multi_network, network_pair_id) {

  # identification of the data sources
  data_source_ids <- id(sp_multi_network,"network_pairs") %[[% network_pair_id
  orig_id <- data_source_ids["orig"]
  dest_id <- data_source_ids["dest"]

  neighborhoods <- pull_neighborhood(sp_multi_network, c(orig_id, dest_id))
  names(neighborhoods) <- c("OW","DW")
  return(neighborhoods)

}

#' @keywords internal
def_matrix_form_args <- function(sp_multi_network, network_pair_id) {

  sp_pair <- pull_pairs(sp_multi_network,network_pair_id)
  flow_completeness <- list(
    "completeness" = prod(nnodes(sp_pair)) / npairs(sp_pair),
    "n_rows" = nnodes(sp_pair,"orig"),
    "n_cols" = nnodes(sp_pair,"dest"),
    "i_rows" = sp_pair %>% dat() %[[% "ORIG_ID" %>% as.integer(),
    "j_cols" = sp_pair %>% dat() %[[% "DEST_ID" %>% as.integer())


  return(flow_completeness)

}



#' @keywords internal
def_spatial_lag_requirements <- function(formula_parts, data_sources) {

  # variable usage by roles
  roles <- c("Y_","G_","O_","D_","I_")
  is_within <- !is.null(data_sources)
  source_role_lookup <- roles_to_sources(is_within)
  role_usage <- formula_parts %>% translist() %[% roles %>% compact()

  what_vars_are_lagged_roles <- function(role_key) {
    source_key <- source_role_lookup[role_key]
    lapply(role_usage[[role_key]], "predict_tranfomed_vars",
           data_sources[[source_key]])
  }
  role_lags <- lookup(names(role_usage)) %>%
    lapply(what_vars_are_lagged_roles) %>% compact()

  # remove lags for Y_ where it does not belong...
  dependent_vars <- role_lags$Y_$norm
  role_lags$G_$norm <- setdiff(role_lags$G_$norm, dependent_vars)
  role_lags$G_$inst <- setdiff(role_lags$G_$inst, dependent_vars)

  return(role_lags)
}

#' @keywords internal
roles_to_sources <- function(is_within) {
  D_source <- if (is_within) "orig" else "dest"
  c("Y_" = "pair","G_" = "pair", "O_" = "orig","D_" = D_source, "I_" = "orig")
}

#' @keywords internal
define_flow_constants <- function(const_formula, use_instruments, OW = NULL) {

  global_const <- 1 %T% const_formula$global

  intra_const <- NULL
  if (const_formula$intra)
    intra_const <- intra_regional_constant(OW, use_instruments)

  return(list("global" = global_const, "intra" = intra_const))
}


#' @importFrom Matrix Diagonal tcrossprod
#' @keywords internal
intra_regional_constant <- function(W, use_instruments = FALSE) {

  In <- list("In" =  Diagonal(nrow(W)) %>% set_instrument_status(FALSE))
  if (!use_instruments)
    return(In)

  V <- tcrossprod(W) # def. V = WW'
  WV <- W %*% V
  WW <- W %*% W
  w_int <- list(
    "W"   = W,
    "W'"  = t(W),
    "WW"  = WW,
    "WW'" = t(WW),
    "V"   = V,
    "VV"  = tcrossprod(WV, W),
    "WV"  = WV,
    "VW'" = t(WV)
  ) %>% lapply("set_instrument_status",TRUE)

  return(c(In,w_int))
}

#' @keywords internal
define_flow_weights <- function(pair_data, weight_var, matrix_form_arguments){

  # When the flows are incomplete and weights weights are not defined, they
  # are used as indicators.
  weights <- weight_var %|!|% (pair_data %[[% weight_var)
  complete_flows <- matrix_form_arguments$completeness == 1
  weights_dont_matter <- is.null(weights) & complete_flows
  if (weights_dont_matter)
    return(NULL)

  args <- c(list("vec" = weights),matrix_form_arguments)
  weights_mat <- do.call(vec_to_matrix,args = args)

  return(weights_mat)
}

### =====OLD==== ----

spflow_model_matrix_old <- function(
  sp_multi_network,
  network_pair_id,
  flow_formula,
  flow_control
) {

  stop("refactor me!")

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

