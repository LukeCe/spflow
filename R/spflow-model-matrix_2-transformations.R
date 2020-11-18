#' @keywords internal
by_source_model_matrix <- function(
  formula_parts,
  data_sources) {

  source_formulas <-
    lapply(formula_parts, function(.f) {
      combine_formulas_by_source(.f,sources = names(data_sources))
    }) %>%
    translist() %>%
    lapply("combine_rhs_formulas")
  source_formulas <- source_formulas[names(data_sources)]

  # nice errors when columns are not available
  plapply(source_formula = source_formulas,
          data_source = data_sources,
          source_type = names(data_sources),
          .f = "validate_source_formulas")

  # Generate model matrices by data source
  source_model_matrices <- plapply(
    formula = source_formulas,
    data = data_sources,
    .f = "flow_conform_model_matrix")


  return(c(source_model_matrices))
}

#' @keywords internal
combine_formulas_by_source <- function(sources, formulas) {

  is_between_flow <- ("dest" %in% sources)
  sources_to_formula_part <- list(
    "pair" = c("Y_","G_"),
    "dest" = c("D_") %T% is_between_flow,
    "orig" = (c("O_") %T% is_between_flow) %||% c("D_","O_","I_")
  ) %>% compact()

  formula_by_source <- sources_to_formula_part %>%
    lapply(function(.part) {
      fpt <- formulas[.part] %>% compact()
      fpt %|!0|% combine_rhs_formulas(fpt) }) %>%
    compact()

  return(formula_by_source)
}

#' @keywords internal
validate_source_formulas <- function(source_formula, data_source,
                                     source_type) {

  required_vars <- all.vars(source_formula %>% combine_rhs_formulas())
  available_vars <- c(colnames(data_source),".")
  unmatched_vars <- required_vars[!required_vars %in% available_vars]

  error_msg <-
    "The variables [%s] were not found in the data set associated to the %s!"

  assert(length(unmatched_vars) == 0,
         error_msg %>%
           sprintf(paste(unmatched_vars,collapse = " and "),
                   c("orig" = "origins",
                     "dest" = "distinations",
                     "pair" = "origin-destination pairs")[source_type]))
}

#' @keywords internal
flow_conform_model_matrix <- function(formula,data) {
  terms_obj <- terms(formula, data = data)
  attr(terms_obj,"intercept") <- 1 - formula_expands_factors(formula,data)
  mat <- model.matrix(terms_obj,data) %>% cols_drop(cols_drop = "(Intercept)")
}
