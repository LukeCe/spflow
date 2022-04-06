#' @title Internal functions to generate model matrices
#' @details
#'   Sources describe the data.frames holding the original information on
#'   the nodes and the node pairs. There are three possible source data.frame
#'   which are referred to as "pair", "orig", or "dest".
#'   All formulas (normal, sdm, and instrument) are combined to generate an
#'   overall model matrix which is only expanded once for each source.
#' @return A list of matrices
#' @keywords internal
flowdata_transformations <- function(
    formula_parts,
    data_sources,
    na_rm,
    weights_var,
    orig_equals_dest) {

  formulas2sources  <- c(
    "D_" = "dest",
    "O_" = "orig",
    "I_" = "orig",
    "G_" = "pair",
    "Y_" = "pair")

  formulas4trans <- translist(formula_parts)[names(formulas2sources)]
  formulas4trans <- lapply(
    formulas4trans[intersect(names(formulas4trans), names(formulas2sources))],
    "combine_rhs_formulas")
  transform_in_source <- function(.key, .na_rm = na_rm) {

    this_formula <- formulas4trans[[.key]]
    if (is.null(this_formula))
      return(NULL)

    this_source <- formulas2sources[.key]
    this_source <- subset_keycols(data_sources[[this_source]], drop_keys = TRUE)
    row.names(this_source) <- NULL
    this_mat <- flow_conform_model_matrix(this_formula, this_source)

    lost_cases <- nrow(this_mat) < nrow(this_source)
    assert(.na_rm || !lost_cases, "
           The transformations specifyed in formula part %s(...) NA values!",
           .key)
    return(this_mat)
  }

  # transform nodes variables
  node_formulas <- c("D_","O_", "I_")
  node_matrices <- lapply(lookup(node_formulas), transform_in_source)

  # ...if there are lost observations in the node matrices
  # ...we can directly remove the corresponding pairs
  do_keys <- get_do_indexes(data_sources[["pair"]])
  do_keys <- get_do_keys(data_sources[["pair"]])
  if (na_rm) {
    rows2index <- function(x) as.integer(row.names(x))
    node_obs_indicators <- lapply(node_formulas, function(.f) {
      obs_source <- nrow(data_sources[[formulas2sources[.f]]])
      obs_trans <- nrow(node_matrices[[.f]])
      if (is.null(obs_trans) || obs_source == obs_trans)
        return(NULL)

      return(rows2index(node_matrices[[.f]]))
    })


    if (!is.null(node_obs_indicators[["D_"]])) {
      keep_dest <- rep(FALSE, nrow(data_sources[["dest"]]))
      keep_dest[node_obs_indicators[["D_"]]] <- TRUE
      keep_dest <- keep_dest[do_keys[,1]]
      do_keys <- do_keys[keep_dest,, drop = FALSE]
      data_sources[["pair"]] <- data_sources[["pair"]][keep_dest,, drop = FALSE]

    }

    if (!is.null(node_obs_indicators[["O_"]])) {
      keep_orig <- rep(FALSE, nrow(data_sources[["orig"]]))
      keep_orig[node_obs_indicators[["O_"]]] <- TRUE
      keep_orig <- keep_orig[do_keys[,2]]
      do_keys <- do_keys[keep_orig,, drop = FALSE]
      data_sources[["pair"]] <- data_sources[["pair"]][keep_orig,, drop = FALSE]
    }

    if (!is.null(node_obs_indicators[["I_"]])) {
      keep_intra <- rep(FALSE, nrow(data_sources[["orig"]]))
      keep_intra[node_obs_indicators[["I_"]]] <- TRUE
      keep_intra <- keep_intra[do_keys[,2]] | keep_intra[do_keys[,1]]
      do_keys <- do_keys[keep_intra,, drop = FALSE]
      data_sources[["pair"]] <- data_sources[["pair"]][keep_intra,, drop = FALSE]
    }
  }


  # transform pair variables
  wt <- weights_var %|!|% data_sources[["pair"]][[weights_var]]
  na_wt <- is.na(wt)
  if (any(na_wt)) {
    assert(na_rm, "The weights contain NA values!")
    do_keys <- cbind(do_keys, wt)[na_wt,,drop = FALSE]
  }

  G_matrices <- transform_in_source("G_")
  if (na_rm && isTRUE(nrow(G_matrices) < nrow(data_sources[["pair"]]))) {
    do_keys <- do_keys[rows2index(G_matrices),,drop = FALSE]
    data_sources[["pair"]] <- data_sources[["pair"]][rows2index(G_matrices),,drop = FALSE]
  }

  Y_matrices <- transform_in_source("Y_", na_rm)
  if (na_rm && isTRUE(nrow(Y_matrices) < nrow(data_sources[["pair"]]))) {
    do_keys <- do_keys[rows2index(Y_matrices),,drop = FALSE]
    G_matrices <- G_matrices[rows2index(Y_matrices),,drop = FALSE]
    data_sources[["pair"]] <- data_sources[["pair"]][rows2index(Y_matrices),,drop = FALSE]
  }


  # matrix format pair variables
  nobs_sources <- unlist(lapply(data_sources,"nrow"))
  non_cartesian <- prod(nobs_sources[c("orig", "dest")]) > nobs_sources["pair"]
  flow_indicator <- matrix_format_d_o(
    dest_index = as.integer(do_keys[,1]),
    orig_index = as.integer(do_keys[,2]),
    num_dest = nobs_sources[["dest"]],
    num_orig = nobs_sources[["orig"]]) %T% non_cartesian

  mat_formatter <- switch(EXPR = class(flow_indicator)
    , "ngCMatrix" = {
      imat <- as(flow_indicator,"dgCMatrix")
      function(vec) {
        imat@x <- vec
        return(imat)}}
    , "matrix" = {
      nd <- nobs_sources[["dest"]]
      no <- nobs_sources[["orig"]]
      mat0 <- matrix(0, nrow = nd, ncol = no)
      pairobs_index <- as.integer(row.names(do_keys))
      function(vec) {
        mat0[pairobs_index] <- vec
        return(mat0)}}
    , "NULL" = {
      nd <- nobs_sources[["dest"]]
      no <- nobs_sources[["orig"]]
      function(vec) {
        return(matrix(vec, nrow = nd, ncol = no))}})

  mform_cols <- function(x) apply(x, 2, mat_formatter, simplify = FALSE)
  G_matrices <- G_matrices %|!|% mform_cols
  Y_matrices <- Y_matrices %|!|% mform_cols
  weights_var <- weights_var %|!|% mat_formatter(do_keys[,3])

  result <- c(node_matrices, list(
    "G_" = G_matrices,
    "Y_" = Y_matrices,
    "weights" = weights_var,
    "flow_indicator" = flow_indicator,
    "do_keys" = do_keys[,1:2, drop = FALSE]))

  return(result)
}

#' @keywords internal
combine_formulas_by_source <- function(sources, formulas) {

  is_between_flow <- ("dest" %in% sources)
  sources_to_formula_part <- list(
    "pair" = c("Y_","G_"),
    "dest" = c("D_") %T% is_between_flow,
    "orig" = (c("O_") %T% is_between_flow) %||% c("D_","O_","I_"))

  formula_by_source <-
    lapply(compact(sources_to_formula_part), function(.part) {
      fpt <- compact(formulas[.part])
      fpt %|!|% combine_rhs_formulas(fpt) })

  return(compact(formula_by_source))
}

#' @keywords internal
flow_conform_model_matrix <- function(formula,data) {
  terms_obj <- terms(formula, data = data)
  attr(terms_obj,"intercept") <- formula_expands_factors(formula,data) * 1
  mat <- model.matrix(terms_obj,data)
  mat[,colnames(mat) != "(Intercept)", drop = FALSE]
}
