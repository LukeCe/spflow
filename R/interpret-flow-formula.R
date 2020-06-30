interpret_flow_formula <- function(
  flow_formula,
  flow_control
) {

  IX <- "IX"
  if (!flow_control$use_intra) IX <- NULL

  relevant_data_sources <- c("DX","OX",IX,"G")
  possible_roles <- c("norm", "sdm", "inst")

  # template the shortcut solution solutions
  formula_shortcuts <-
    list("all"  = (~ . - 1) %>% named_list(relevant_data_sources, .),
         "same" = split_flow_formula(flow_formula)[relevant_data_sources])

  ## "Normal variables" (which appear in any case)
  decomposed_formulas <- named_list(possible_roles)
  decomposed_formulas$"norm" <-
    c(list("Y" = pull_lhs(flow_formula) %>% remove_intercept()),
      formula_shortcuts$same)

  ## SDM variables (which appear in the SDM model only)
  if (!flow_control$use_sdm)
    decomposed_formulas$sdm <- NULL

  if (flow_control$use_sdm) {
    relevant_sources <- c("DX","OX",IX)

    decomposed_formulas$sdm <-
      split_flow_formula(flow_control$sdm_variables) %||%
      formula_shortcuts[[flow_control$sdm_variables]] %>%
      `[`(relevant_sources)
  }

  ## Instrumental variables (which appear only in s2sls estimation)
  if (flow_control$estimation_method != "s2sls")
    decomposed_formulas$inst <- NULL

  if (flow_control$estimation_method == "s2sls") {
    relevant_sources <- c("DX","OX",IX ,"G")

    decomposed_formulas$inst <-
      split_flow_formula(flow_control$instrumental_variables) %||%
      formula_shortcuts[[flow_control$instrumental_variables]] %>%
      `[`(relevant_sources)
  }

  return(decomposed_formulas)
}

split_flow_formula <- function(main_formula) {

  if (!is(main_formula,"formula"))
    return(NULL)

  possible_data_sources <- c("DX","OX","IX","G")
  special_formulas      <- c("D" ,"O" ,"I" ,"G") %p% "_"

  split_formulas <- extract_specials(
    formula = main_formula %>% pull_rhs(),
    specials = special_formulas)

  names(split_formulas) <- possible_data_sources

  return(split_formulas)
}


extract_specials <- function(formula, specials) {


  terms_formula <- terms.formula(
    formula,
    specials = specials,
    data = data.frame("." = "."))

  # determine the variables for specific cases
  all_varibales <- rownames(attr(terms_formula, "factors"))

  specific_vars_indexes <-
    attr(terms_formula,"specials") %>%
    compact()

  # assign the specials to a function
  fun_env <- environment()
  specials %>%
    lapply(assign,
           value = function(.s) to_rhs_formula(deparse(substitute(.s))),
           envir = fun_env)

  # apply the function which just parses its arguments as a fromula
  specific_formulas <-
    specific_vars_indexes %>%
    lapply(function(.i) eval(parse(text = all_varibales[.i]))) %>%
    lapply(remove_intercept)


  # determine the generic variables used for all remaining cases
  drop_specials <- c(unlist(specific_vars_indexes),
                     length(all_varibales) + 1)
  generic_vars <- all_varibales[-drop_specials]

  generic_formula <- NULL
  if (length(generic_vars) > 0) {
    generic_formula <- to_rhs_formula(generic_vars) %>% remove_intercept()
  }

  # default to generic formula for all cases
  # then overwrite special cases
  case_formulas <- named_list(specials, generic_formula)
  case_formulas[names(specific_formulas)] <- specific_formulas

  return(case_formulas)

}
