#' @keywords internal
interpret_spflow_formula <- function(
  spflow_formula,
  spflow_control) {

  ### ---- split according to specials and treat constants first
  all_specials <- c("D_","O_","I_","P_")
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
  norm_f <- c("D_", "O_", I_, "P_")
  sdm_f  <- c("D_", "O_", I_) %T% has_sdm
  inst_f <- c("D_", "O_", I_, "P_") %T% has_inst

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

# ---- formula primitives -----------------------------------------------------
assert_formula <- function(formula) {
  assert_is(formula,"formula")
}

#' @keywords internal
is_one_sided_formula <- function(formula) {
  is(formula,"formula") & (length(formula) == 2)
}

#' @keywords internal
is_two_sided_formula <- function(formula) {
  is(formula,"formula") & (length(formula) == 3)
}

#' @keywords internal
has_constant <- function(formula) {
  assert_formula(formula)
  attr(terms(formula,allowDotAsName = TRUE),"intercept") == 1
}

#' @keywords internal
has_dot_shortcut <- function(formula) {
  "." %in% extract_formula_terms(formula)
}

#' @keywords internal
data_permits_formula <- function(formula,data) {
  assert_formula(formula)
  stopifnot(inherits(data, "data.frame"))
  data <- data[0, ,drop = FALSE]

  possible <- tryCatch(
    expr = {is(model.matrix(formula, data = data),"matrix")},
    error = function(e) FALSE)

  if (possible) TRUE else FALSE
}

#' @keywords internal
formula_expands_factors <- function(formula,data) {
  assert_formula(formula)
  stopifnot(is.data.frame(data))
  data <- data[0, ,drop = FALSE]

  no_fct <-  attr(model.matrix(formula, data = data), "contrasts")
  return(!is.null(no_fct))
}

# ---- reshaping the formula --------------------------------------------------
#' @keywords internal
compact_formula_internal <- function(formula, keep_const = TRUE) {
  assert_formula(formula)

  compact_rhs <- extract_formula_terms(formula) %||% "1"
  if (length(compact_rhs) == 0)
    return(formula)

  compact_lhs <- NULL
  if (is_two_sided_formula(formula))
    compact_lhs <- extract_formula_terms(pull_lhs(formula))

  compact_formula <-
    reformulate(compact_rhs,
                intercept = keep_const & has_constant(formula),
                response = compact_lhs)

  return(compact_formula)
}

#' @keywords internal
compact_formula <- function(formula) {
  compact_formula_internal(formula,keep_const = TRUE)
}

#' @keywords internal
remove_constant <- function(formula) {
  compact_formula_internal(formula,keep_const = FALSE)
}

#' @keywords internal
combine_rhs_formulas <- function(...) {

  rhs_formulas <- flatlist(list(...))
  rhs_formulas <- lapply(compact(rhs_formulas), "pull_rhs")
  use_constant <- all(sapply(rhs_formulas , "has_constant"))

  combined_formula <- unlist(lapply(rhs_formulas, extract_formula_terms))
  combined_formula <- reformulate(unique(combined_formula),
                                  intercept = use_constant)
  return(combined_formula)
}

#' @keywords internal
pull_rhs <- function(formula) {
  assert_formula(formula)
  return_rhs <- formula
  if (is_two_sided_formula(formula)) return_rhs <- return_rhs[c(1,3)]

  return(return_rhs)
}

#' @keywords internal
pull_lhs <- function(formula) {
  assert(is_two_sided_formula(formula),
         "The input musst be a two sided formula!")
  return(formula[c(1,2)])
}


# ---- accessing formula elements ---------------------------------------------
#' @keywords internal
extract_formula_terms <- function(formula, data = NULL) {
  assert_formula(formula)

  if (is.null(data))
    return(labels(terms(formula, allowDotAsName = TRUE)))

  assert_is(data,"data.frame")
  return(labels(terms(formula, data = data)))
}


# ---- fine tuned expansions of the formula -----------------------------------
#' @keywords internal
extract_transformed_varnames <- function(formula,data) {
  data <- data[0, ,drop = FALSE]
  assert(data_permits_formula(formula,data),
         "The formula cannot be applied to the data!")

  # add intercept to have predictable factor expansions
  terms_obj <- terms(formula, data = data)
  attr(terms_obj,"intercept") <- 1
  dummy_matrix <- model.matrix(terms_obj,data)
  trans_vars <- colnames(dummy_matrix)

  # were there factors?
  used_factor <- names(attr(dummy_matrix,"contrasts"))
  expanded_factor <- NULL
  if (!is.null(used_factor)) {
    fact_index_pre <- which(attr(terms_obj,"term.labels") %in% used_factor)
    fact_index_trans <- which(attr(dummy_matrix,"assign") %in% fact_index_pre)
    expanded_factor <- trans_vars[fact_index_trans]
  }

  result <- compact(list(
    "names" = setdiff(trans_vars, "(Intercept)" %T% !has_constant(formula)),
    "factors" = expanded_factor))

  return(result)
}

#' @keywords internal
predict_tranfomed_vars <- function(formula,data) {
  setdiff(extract_transformed_varnames(formula,data)$names, "(Intercept)")
}

#' @keywords internal
predict_expanded_factors <- function(formula,data) {
  extract_transformed_varnames(formula,data)$factors
}

#' @keywords internal
extract_formula_specials <- function(formula,specials) {

  # split all terms into special or general
  terms_obj_formula <- terms.formula(formula, specials, allowDotAsName = TRUE)
  all_terms <-  rownames(attr(terms_obj_formula, "factors"))

  special_terms <- lapply(attr(terms_obj_formula,"specials"),
                          function(.s_index) {all_terms[.s_index]})
  non_special_terms <- setdiff(all_terms,unlist(special_terms))

  return(list("specials" = special_terms, "normals" = non_special_terms))
}

# ---- split the formula by "special" functions -------------------------------
#' @keywords internal
split_formula_specials <- function(
    formula, specials) {
  assert_formula(formula)
  assert_is(specials,"character")

  split_terms <- extract_formula_specials(formula,specials)

  # first create the normal formula
  nt <- split_terms$normals
  normal_formula <- nt %|!|% reformulate(nt,intercept = has_constant(formula))

  # then create the special formulas
  st <- split_terms$specials
  special_formulas <- st %|!|%
    Map("special_formula_as_rhs", special = names(st), string_formula = st)

  null_special <- unlist(lapply(special_formulas,is.null))
  if (any(null_special)) {
    all_generals <- named_list(specials,normal_formula)
    special_formulas[null_special] <- all_generals[null_special]
  }


  return(special_formulas)
}

#' @keywords internal
special_formula_as_rhs <- function(special,string_formula) {
  if (length(nchar(string_formula)) == 0) return(NULL)

  # make the special a function
  fun_env <- environment()
  assign(x = special, parse_fun_arguments_as_string, envir = fun_env)

  # evaluating the special -> returns its arguments a string
  special_rhs_formula <- eval_string_as_function(string_formula, e = fun_env)
  special_rhs_formula <- paste(special_rhs_formula, collapse = "")
  return(reformulate_string(special_rhs_formula))
}

#' @keywords internal
reformulate_string <- function(string_formula) {
  # catch empty string or only white spaces
  if ("" == gsub("\\s.*",replacement = "",x = string_formula))
    string_formula <- "1"

  result <- reformulate(string_formula)
  result <- compact_formula(result)
  return(result)
}

#' @keywords internal
parse_fun_arguments_as_string <- function(x){
  deparse(substitute(x))
}

#' @keywords internal
eval_string_as_function <- function(x,e = environment()){
  eval(parse(text = x),envir = e)
}

