#' Expand the structural formula of a spatial interaction model
#'
#' Decomposes the structural formulas of a spatial interaction model into one
#' sided formulas which are then applied to five cases and in three different
#' roles.
#' + 5 cases: **Interactions ~ Origins + Destinations + Intra + Pairs**
#' + 3 roles: **normal_variables & sdm_variables & instrumental_variables**
#'
#'
#' @details
#' A spatial interaction model has potentially multiple data sources and
#' multiple roles for each variable.
#' The five cases refer to the type of data notably:
#'   1. response variable (interactions)
#'   2. attributes of origins
#'   3. attributes of destination
#'   4. attributes of intra-observational units
#'   5. attributes or origin-destination pars (such as distance)
#'
#' The three possible roles refer to the use of a variable as:
#'   1. normal variable
#'   2. sdm variable (as spatial lag)
#'   3. instrumental variable (to use during s2sls estimation)
#'
#' @param flow_formula A formulas corresponding to the structural interaction model
#' @param flow_control A [spflow_control()] list to fine tune the estimation
#'
#' @keywords internal
#' @return
model_formula_decompose <- function(
  flow_formula,
  flow_control
) {

  role_and_case_formulas <-
    list(
      "normal_variables" = expand_main_formula(flow_formula),
      "sdm_variables" = {
        expand_role_formula(
          main_formula = flow_formula,
          role_formula = flow_control[["sdm_variables"]],
          use_pairs = FALSE,
          use_intra = flow_control$use_intra)
        },
      "instrumental_variables" = {
        expand_role_formula(
          main_formula = flow_formula,
          role_formula = flow_control[["instrumental_variables"]],
          use_pairs = TRUE,
          use_intra = flow_control$use_intra)
      }
    )

  # FIXME hack to solve intra formula problem
  if (!flow_control$use_intra) {
    role_and_case_formulas$normal_variables$intra_ <- ~ -1
  }

  return(role_and_case_formulas)
}

expand_role_formula <- function(
  main_formula,
  role_formula,
  use_pairs,
  use_intra
  ) {


  use_cases <- as.vector(define_case_prefixes())

  # sdm never required for pairs
  # intra is optional
  null_roles <- structure(rep(FALSE,length(use_cases)), names = use_cases)
  null_roles["intra_"] <- !use_intra
  null_roles["pair_"] <- !use_pairs

  # early returns for shortcuts notations
  if (role_formula == "all") {
    return(named_list(use_cases, ~ . -1))
  }

  null_formulas <- named_list(use_cases, ~ -1)
  if (role_formula == "none") {
    return(null_formulas)
  }

  if (role_formula == "same") {
    role_formulas <- expand_main_formula(main_formula)[use_cases]
    role_formulas[null_roles] <- null_formulas[null_roles]
    return(role_formulas)
  }

  assert(is_one_sided_formula(role_formula),
         "The declaration of variable roles musst be a one sided formula " %p%
         "or one of the keywords [all, none, same]!")
  role_formulas <- expand_case_formulas(role_formula)
  return(role_formulas)
}



expand_main_formula <- function(f) {

  assert(is_two_sided_formula(f),
         "The response variables musst be declared explicitly!")

  flow_formulas <-
    list("interactions" = pull_lhs(f) %>% remove_intercept()) %>%
    append(.,expand_case_formulas(pull_rhs(f)))

  return(flow_formulas)
}

#' Decomposes the formula provided to [spflow()] into four parts.
#'
#' The sub-formulas provided by this decomposition can be carried to the
#' different data sources for exogenous data (origin data, destination data, pair data),
#' which permits to use a formula interface similar to the one provided by [lm()].
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

  generic_formula <- NULL
  if (length(generic_vars) > 0) {
    generic_formula <- to_rhs_formula(generic_vars) %>% remove_intercept()
  }

  # default to generic formula for all cases
  case_formulas <- named_list(cases, generic_formula)
  # overwrite special cases
  has_specific <- case_prefixes[names(specific_formulas)]
  case_formulas[has_specific] <- specific_formulas

  return(case_formulas)

}


D_ <- O_ <- I_ <- G_ <- function(l) to_rhs_formula(deparse(substitute(l)))
define_case_prefixes <- function() {
  c("O_"  = "orig_",
    "D_"  = "dest_",
    "I_"  = "intra_",
    "G_"  = "pair_")
}

define_variable_roles <- function() {
  c("normal","sdm","instrumental") %p% "_variable"
}
