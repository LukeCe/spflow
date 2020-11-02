interpret_flow_formula <- function(
  flow_formula,
  flow_control
) {

  # TODO think how to remove the constant terms...
  IX <- "IX"
  if (!flow_control$use_intra) IX <- NULL

  relevant_data_sources <- c("DX","OX",IX,"G")
  possible_roles <- c("norm", "sdm", "inst")

  # template the shortcut solution solutions
  formula_shortcuts <-
    list("all"  = (~ . - 1) %>% named_list(names = relevant_data_sources),
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

  split_formulas <- try({
    extract_specials(formula = main_formula %>% pull_rhs(),
                     specials = special_formulas)
      },silent = TRUE)

  assert(!is(split_formulas, "try-error"),
         error_msg = stop("The specifyed flow_formula can not be interpreted."))


  names(split_formulas) <- possible_data_sources

  return(split_formulas)
}


extract_specials <- function(formula, specials) {


  terms_formula <- terms.formula(
    formula,
    specials = specials,
    data = data.frame("." = "."))

  # assign the specials to a function
  # that returns its content as a one sided formula
  # >> is evaluated below using parse...
  fun_env <- environment()
  specials %>%
    lapply(assign, value = function(.s) {
             to_rhs_formula(paste(deparse(substitute(.s)),collapse = ""))
           },
           envir = fun_env)

  # determine the variables for specific cases
  all_variables <- rownames(attr(terms_formula, "factors"))

  specific_vars_indexes <-
    attr(terms_formula,"specials") %>%
    compact()

  # preasign_secific formulas: for each index there must be one
  specific_formulas <- named_list(names(specific_vars_indexes), ~ -1)

  # filter out empty specials for shortcut notation
  empty_special <-
    all_variables[unlist(specific_vars_indexes)] %in% (specials %p% "()")

  # evaluates a string as if it was a function
  # >> the previously assigned function
  specific_formulas_temp <-
    specific_vars_indexes[!empty_special] %>%
    lapply(function(.i) eval(parse(text = all_variables[.i]))) %>%
    lapply(remove_intercept)

  specific_formulas[names(specific_formulas_temp)] <- specific_formulas_temp


  # determine the generic variables used for all remaining cases
  drop_specials <- c(unlist(specific_vars_indexes),
                     length(all_variables) + 1)
  generic_vars <- all_variables[-drop_specials]

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
