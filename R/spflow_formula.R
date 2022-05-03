#' @keywords internal
interpret_spflow_formula <- function(
  spflow_formula,
  spflow_control) {

  ### ---- split according to specials and treat constants first
  all_specials <- c("D_","O_","I_","G_")
  split_specials <- split_formula_specials(pull_rhs(spflow_formula),all_specials)
  constants <- compact(list(
    "global" = spflow_formula,
    "intra" =  split_specials$I_ %||% spflow_formula %T% spflow_control$use_intra))
  constants <- lapply(constants, "has_constant")

  ### ---- split the right hand side formulas for all three cases...
  # define the parts of the formula that are relevant for each case
  has_sdm <- spflow_control[["sdm_variables"]] != "none"
  has_inst <- spflow_control[["estimation_method"]] == "s2sls"
  usa_sdm_shortcut <- !is.character(spflow_control[["sdm_variables"]])

  I_ <- "I_" %T% spflow_control$use_intra
  norm_f <- c("D_", "O_", I_, "G_")
  sdm_f  <- c("D_", "O_", I_) %T% has_sdm
  inst_f <- c("D_", "O_", I_, "G_") %T% has_inst

  # derive the split formulas
  norm_rhs_split <- compact(split_specials[norm_f])
  norm_lhs <- list("Y_" = pull_lhs(spflow_formula))
  sdm_rhs_split  <- sdm_f %|!|%
    split_with_shortcut(spflow_control[["sdm_variables"]],
                        sdm_f,norm_rhs_split)

  # sdm shortcuts don not apply to intra
  sdm_rhs_split$I_ <- sdm_rhs_split$I_ %T% usa_sdm_shortcut


  inst_rhs_split <- inst_f %|!|%
    split_with_shortcut(spflow_control[["twosls_instrumental_variables"]],
                        inst_f, norm_rhs_split)

  ### ---- assemble all formulas with constants set apart
  strip_consts <- function(.ll) lapply(compact(.ll),"remove_constant")
  strip_empty <- function(.ll) {
    Filter(function(.l) length(extract_formula_terms(.l)) != 0, .ll)
    }

  spflow_formulas_decomposed <- list(
    "norm" = c(norm_lhs,norm_rhs_split),
    "sdm" = sdm_rhs_split,
    "inst" = inst_rhs_split)
  spflow_formulas_decomposed <-
    lapply(compact(spflow_formulas_decomposed), "strip_consts")
  spflow_formulas_decomposed <-
    lapply(compact(spflow_formulas_decomposed), "strip_empty")

  return(c(list("constants" = constants),
           spflow_formulas_decomposed))
}

#' @keywords internal
split_with_shortcut <- function(case_formula,case_specials,rhs_norm){

  has_shortcut <- is.character(case_formula)
  if (!has_shortcut)
    return(split_formula_specials(case_formula,case_specials))

  result <- switch (case_formula,
    "all" = named_list(case_specials, ~ .),
    "same" = rhs_norm[case_specials],
    "none" = NULL)

  return(result)
}
