#' @title Internal functions to generate model matrices
#' @details
#'   Sources describe the data.frames holding the original information on
#'   the nodes and the node pairs. There are three possible source data.frame
#'   which are referred to as "pair", "orig", or "dest".
#'   All formulas (normal, sdm, and instrument) are combined to generate an
#'   overall model matrix which is only expanded once for each source.
#' @return A list of matrices
#' @keywords internal
by_source_variable_trans <- function(
  formula_parts,
  data_sources,
  weights_var = NULL,
  ignore_na = FALSE,
  is_within = FALSE) {

  source_formulas <- lapply(formula_parts, function(.f) {
    combine_formulas_by_source(sources = names(data_sources),
                               formulas = .f)})
  source_formulas <- lapply(translist(source_formulas), "combine_rhs_formulas")
  source_formulas <- source_formulas[names(data_sources)]

  # nice errors when columns are not available
  Map("validate_source_formulas",
      source_formula = source_formulas,
      data_source = data_sources,
      source_type = names(data_sources))

  # Generate model matrices by data source
  data_sources <- lapply(data_sources, "row.names<-", NULL)
  source_model_matrix <- function(.s) flow_conform_model_matrix(
    source_formulas[[.s]],
    subset_keycols(data_sources[[.s]], drop_keys = TRUE))

  orig_mm <- source_model_matrix("orig")
  dest_mm <- source_model_matrix("dest")
  lost_origs <- nrow(orig_mm) != nrow(data_sources[["orig"]])
  lost_dests <- nrow(dest_mm) != nrow(data_sources[["dest"]])
  if (!ignore_na) {
    error_msg <- "
    There are missing values in the data associcated with the %s,
    please remove them from the data first!"
      assert(!lost_origs, error_msg, "origins")
      assert(!lost_dests, error_msg, "destinations")
  }

  # when weights have NA's remove them first
  wt <- weights_var %|!|% data_sources[["pair"]][[weights_var]]
  if (!is.null(wt)) {
    wt <- is.finite(wt)
    if (any(!wt)) {
      assert(ignore_na, "NA/NaN/Inf in weights!")
      data_sources[["pair"]] <- data_sources[["pair"]][wt,,drop = FALSE]
    }
  }

  # when origins or destinations are missing in the node data
  # remove them from the pair data before transforming it
  if (lost_origs) {
    obs_nodes <- rep(TRUE, nrow(data_sources[["orig"]]))
    obs_nodes[-as.integer(row.names(orig_mm))] <- FALSE

    orig_key <- attr_key_orig(data_sources[["pair"]])
    obs_origs <- as.integer(data_sources[["pair"]][[orig_key]])
    obs_origs <- obs_nodes[obs_origs]
    data_sources[["pair"]] <- data_sources[["pair"]][obs_origs,]
  }
  if (lost_dests) {
    obs_nodes <- rep(TRUE, nrow(data_sources[["dest"]]))
    obs_nodes[-as.integer(row.names(dest_mm))] <- FALSE

    dest_key <- attr_key_dest(data_sources[["pair"]])
    obs_dests <- as.integer(data_sources[["pair"]][[dest_key]])
    obs_dests <- obs_nodes[obs_dests]
    data_sources[["pair"]] <- data_sources[["pair"]][obs_dests,]
  }

  dest_is_redundant <-
    is_within && all(row.names(orig_mm) == row.names(dest_mm))
  if (dest_is_redundant) {
    dest_specific_cols <- setdiff(colnames(dest_mm), colnames(orig_mm))
    orig_mm <- cbind(orig_mm,dest_mm[,dest_specific_cols, drop = FALSE])
    dest_mm <- NULL
  }

  pair_mm <- source_model_matrix("pair")
  lost_pairs <- nrow(pair_mm) != nrow(data_sources[["pair"]])
  assert(!lost_pairs | ignore_na, error_msg, "origin-destination pairs")

  source_model_matrices <- named_list(c("pair", "orig", "dest"))
  source_model_matrices[["pair"]] <- pair_mm
  source_model_matrices[["orig"]] <- orig_mm
  source_model_matrices[["dest"]] <- dest_mm

  return(source_model_matrices)
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
validate_source_formulas <- function(source_formula, data_source,
                                     source_type) {

  required_vars <- all.vars(combine_rhs_formulas(source_formula))
  available_vars <- c(colnames(data_source),".")
  unmatched_vars <- required_vars[!required_vars %in% available_vars]

  error_msg <-
    "The variables [%s] were not found in the data set associated to the %s!"

  assert(length(unmatched_vars) == 0,
         sprintf(error_msg,
                 paste(unmatched_vars,collapse = " and "),
                   c("orig" = "origins",
                     "dest" = "distinations",
                     "pair" = "origin-destination pairs")[source_type]))
}

#' @keywords internal
flow_conform_model_matrix <- function(formula,data) {
  terms_obj <- terms(formula, data = data)
  attr(terms_obj,"intercept") <- formula_expands_factors(formula,data) * 1
  mat <- model.matrix(terms_obj,data)
  mat[,colnames(mat) != "(Intercept)", drop = FALSE]
}
