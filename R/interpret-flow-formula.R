#' @keywords internal
interpret_flow_formula <- function(
  flow_formula,
  flow_control
) {
  #### define relevant special functions (intra or not)
  intra_special <- "I_" %T%  flow_control$use_intra
  all_specials <- c("D_","O_",intra_special,"G_")

  #### split the right hand side for all three cases...
  # ...1.) normal
  norm_rhs_split <- split_forumla_specials(pull_rhs(flow_formula),all_specials)

  # ... 2.) sdm (overwrite null if needed)
  sdm_rhs_split <- split_with_shortcut(
    flow_control$sdm_variables,
    setdiff(all_specials,"G_"), # G_(.) are never used as sdm vars
    norm_rhs_split) %T% flow_control$use_sdm

  # ...3.) inst (overwrite null if needed)
  use_inst <- flow_control$estimation_method == "s2sls"
  inst_rhs_split <- split_with_shortcut(
    flow_control$instrumental_variables,
    all_specials,
    norm_rhs_split) %T% use_inst

  #### treat the constant terms and the lhs
  constants <- list(
    "global" = has_constant(flow_formula),
    "intra" = norm_rhs_split$I_ %|!|% has_constant(norm_rhs_split$I_ )
    ) %>% compact()

  norm_lhs <- list("Y_" = pull_lhs(flow_formula))

  #### assemble all formulas
  flow_formulas_decomposed <- list(
    "const" = constants,
    "norm" = c(norm_lhs,norm_rhs_split) %>% lapply("remove_constant"),
    "sdm" = sdm_rhs_split,
    "inst" = inst_rhs_split
  ) %>% compact()

  return(flow_formulas_decomposed)
}

#' @keywords internal
split_with_shortcut <- function(case_formula,case_specials,rhs_norm){

  has_shortcut <- is.character(case_formula)
  if (!has_shortcut)
    return(split_forumla_specials(case_formula,case_specials))

  result <- switch (case_formula,
    "all" = named_list(case_specials, ~ . - 1),
    "same" = lapply(rhs_norm[case_specials], "remove_constant"),
    "none" = NULL)

  return(result)
}
