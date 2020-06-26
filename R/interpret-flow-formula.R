interpret_flow_formula <- function(
  flow_formula,
  flow_control
) {

  IX <- "IX"
  if (!flow_control$use_intra) IX <- NULL

  relevant_data_sources <- c("Y","DX","OX",IX,"G")
  possible_roles <- c("norm", "sdm", "inst")

  ## "Normal variables" (which appear in any case)
  decomposed_formulas <- named_list(possible_roles)
  decomposed_formulas$norm <-
    split_flow_formula(flow_formula)[relevant_data_sources]

  flow_formulas <-
    list("interactions" = pull_lhs(f) %>% remove_intercept()) %>%
    append(.,expand_case_formulas(pull_rhs(f)))
  # template the shortcut solution solutions
  formula_shortcuts <-
    list("all"  = (~ . - 1) %>% named_list(relevant_data_sources, .),
         "same" = decomposed_formulas$norm)

  ## SDM variables (which appear in the SDM model only)
  if (!flow_control$use_sdm)
    decomposed_formulas$sdm <- NULL

  if (flow_control$use_sdm) {

    decomposed_formulas$sdm <-
      split_flow_formula(flow_control$sdm_variables) %||%
      formula_shortcuts[flow_control$sdm_variables]

    relevant_sources <- c("DX","OX",IX)
    decomposed_formulas$sdm <- decomposed_formulas$sdm[relevant_sources]
  }

  ## Instrumental variables (which appear only in s2sls estimation)
  if (flow_control$estimation_method != "s2sls")
    decomposed_formulas$inst <- NULL

  if (flow_control$estimation_method == "s2sls") {

    decomposed_formulas$inst <-
      split_flow_formula(flow_control$instrumental_variables) %||%
      formula_template[flow_control$instrumental_variables]

    relevant_sources <- c("DX","OX",IX ,"G")
    decomposed_formulas$inst <- decomposed_formulas$inst[relevant_sources]
  }

  return(decomposed_formulas)

}


split_flow_formula <- function(main_formula) {

  if (!is_one_sided_formula(main_formula))
    return(NULL)

  possible_data_sources <- c("Y","OX","DX","IX","G")

  special_formulas <- c("D","O","I","G") %p% "_"
  D_ <- O_ <- I_ <- G_ <- function(l) to_rhs_formula(deparse(substitute(l)))




}

