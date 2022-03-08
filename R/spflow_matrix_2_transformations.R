#' @title Internal functions to generate model matrices
#' @details
#'   Sources describe the data.frames holding the original information on
#'   the nodes and the node pairs. There are three possible source data.frame
#'   which are referred to as "pair", "orig", or "dest".
#'   All formulas (normal, sdm, and instrument) are combined to generate an
#'   overall model matrix which is only expanded once for each source.
#' @return A list of matrices
#' @keywords internal
by_source_model_matrix <- function(
  formula_parts,
  data_sources) {

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
  source_model_matrices <- Map(
    "flow_conform_model_matrix",
    formula = source_formulas,
    data = data_sources)

  no_drops <- sapply(source_model_matrices, nrow) == sapply(data_sources, nrow)
  if (all(no_drops))
    return(c(source_model_matrices))

  error_source <- names(no_drops[!no_drops])
  error_source <- c("pair" = "oriigin-destination pairs",
                    "orig" = "origins",
                    "dest" = "destinations")[error_source]
  error_source <- paste0(error_source, collapse =  " and ")
  error_msg <- "
  There are missing values in the data associcated with the %s,
  please remove them from the data first!
  <br>Also make sure that no missing values are created through the
  transformations in your formula (e.g logartihms of negative numbers)."
  stop(sprintfwrap(error_msg, error_source))

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
