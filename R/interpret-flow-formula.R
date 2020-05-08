interpret_roles_and_cases <- function(
  forumula,
  control
)

derive_role_formulas <- function(
  main_formula,
  case_formulas,
  role,
  control
) {

  # sdm not required for pairs
  # intra only when required
  use_cases <- as.vector(define_case_prefixes())
  null_roles <- structure(rep(FALSE,length(use_cases)), names = use_cases)
  null_roles["Intra_"] <- use_intra
  null_roles["Pair_"] <- (role == "sdm_variables")

  # early returns for shortcuts notations
  role_control <- control[[role]]
  if (role_control == "all") {
    return(named_list(use_cases, ~ . + 0))
  }

  null_formulas <- named_list(use_cases, ~0)
  if (role_control == "none") {
    return(null_formulas)
  }

  if (role_control == "same") {
    role_formulas <- case_formulas[cases]
    role_formulas[null_roles] <- null_formulas[null_roles]
    return(role_formulas)
  }

  role_formulas <- expand_case_formulas(pull_rhs(main_formula))
  return(role_formulas)

}



expand_flow_formula <- function(f) {

  assert(is_two_sided_formula(f),
         "The response variables musst be declared explicitly!")

  flow_formulas <-
    list("interactions" = pull_lhs(f)) %>%
    append(.,expand_case_formulas(pull_rhs(f)))

  return(flow_formulas)
}

#' Decomposes the formula provided to [spflow()] into four parts.
#'
#' The sub-formulas provided by this decomposition can be carried to the
#' diffrent data sources for exogenous data (origin data, destination data, pair data),
#' which permits to use a formular interface similar to the one provided by [lm()].
#'
#' @param formula A formula which specifies the structural model
#'
#' @return A list of formulas for each data source.
#' @keywords internal
expand_case_formulas <- function(rhs_formula) {

  case_prefixes <- define_case_prefixes()
  cases <- as.vector(case_prefixes)

  # Non wrapped arguments will be carryed to all cases
  role_specials <- names(case_prefixes)

  dummy_frame <- data.frame("." = ".")
  terms_formula <- terms.formula(rhs_formula,
                                 specials = role_specials,
                                 data = dummy_frame)

  # determine the variables for specific cases
  all_varibales <- rownames(attr(terms_formula, "factors"))
  specific_vars_indexes <- attr(terms_formula,"specials") %>% compact()
  specific_formulas <-
    lapply(specific_vars_indexes,
           function(.i) eval(parse(text = all_varibales[.i]))) %>%
    lapply(remove_intercept)


  # determine the generic variables used for all remaining cases
  drop_specials <- c(unlist(specific_vars_indexes),
                     length(all_varibales) + 1)
  generic_vars <- all_varibales[-drop_specials]
  generic_formula <- to_rhs_formula(generic_vars) %>% remove_intercept()

  # default to generic formula for all cases
  case_formulas <- named_list(cases, generic_formula)
  # overwrite special cases
  has_specific <- case_prefixes[names(specific_formulas)]
  case_formulas[has_specific] <- specific_formulas

  return(case_formulas)

}


D_ <- O_ <- I_ <- G_ <- function(l) to_rhs_formula(deparse(substitute(l)))
define_case_prefixes <- function() {
  c("O_"  = "Orig_",
    "D_"  = "Dest_",
    "I_"  = "Intra_",
    "G_"  = "Pair_")
}

