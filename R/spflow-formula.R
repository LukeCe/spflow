#' @keywords internal
interpret_flow_formula <- function(
  flow_formula,
  flow_control
) {

  ### ---- split according to specials and treat constants first
  all_specials <- c("D_","O_","I_","G_")
  split_specials <- split_forumla_specials(pull_rhs(flow_formula),all_specials)
  constants <- list(
    "global" = flow_formula,
    "intra" =  split_specials$I_ %||% flow_formula %T% flow_control$use_intra
  ) %>% compact() %>% lapply("has_constant")

  ### ---- split the right hand formulas side for all three cases...

  # define the parts of the formula that are relevant for each case
  norm_f <- c("D_","O_","I_" %T%  flow_control$use_intra,"G_")
  sdm_f <- setdiff(norm_f,"G_") %T% flow_control$use_sdm
  inst_f <- norm_f %T% (flow_control$estimation_method == "s2sls")

  # derive the split formulas
  norm_rhs_split <- split_specials[norm_f] %>% compact()
  norm_lhs <- list("Y_" = pull_lhs(flow_formula))
  sdm_rhs_split  <- sdm_f %|!|%
    split_with_shortcut(flow_control$sdm_variables, sdm_f,norm_rhs_split)
  inst_rhs_split <- inst_f %|!|%
    split_with_shortcut(flow_control$instrumental_variables, inst_f,
                        norm_rhs_split)

  ### ---- assemble all formulas with constants set apart
  strip_consts <- function(.ll) lapply(compact(.ll),"remove_constant")
  strip_empty <- function(.ll) {
    lfilter(.ll, function(.l) length(extract_formula_terms(.l)) != 0)}


  flow_formulas_decomposed <- list(
    "norm" = c(norm_lhs,norm_rhs_split),
    "sdm" = sdm_rhs_split,
    "inst" = inst_rhs_split
  ) %>% compact() %>% lapply("strip_consts") %>% lapply("strip_empty")


  return(c(list("constants" = constants),
           flow_formulas_decomposed))
}

#' @keywords internal
split_with_shortcut <- function(case_formula,case_specials,rhs_norm){

  has_shortcut <- is.character(case_formula)
  if (!has_shortcut)
    return(split_forumla_specials(case_formula,case_specials))

  result <- switch (case_formula,
    "all" = named_list(case_specials, ~ .),
    "same" = rhs_norm[case_specials],
    "none" = NULL)

  return(result)
}