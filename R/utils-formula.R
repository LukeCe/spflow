# ---- formula primitives -----------------------------------------------------
#' @keywords internal
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
  stopifnot(is.data.frame(data))
  data <- data[0,]

  possible <- tryCatch(
    expr = {is(model.matrix(formula, data = data),"matrix")},
    error = function(e) FALSE)

  if (possible) TRUE else FALSE
}

# ---- reshaping the formula --------------------------------------------------
#' @keywords internal
compact_formula <- function(formula) {
  assert_formula(formula)

  compact_rhs <- extract_formula_terms(formula)
  compact_lhs <- NULL
  if (is_two_sided_formula(formula))
    compact_lhs <- extract_formula_terms(pull_lhs(formula))

  compact_formula <-
    reformulate(compact_rhs,
                intercept = has_constant(formula),
                response = compact_lhs)

  return(compact_formula)
}

#' @keywords internal
combine_rhs_formulas <- function(...) {

  rhs_formulas <- list(...) %>% flatlist() %>% lapply("pull_rhs")
  use_constant <- all(rhs_formulas %>% lapply("has_constant") %>% unlist())

  combined_formula <- rhs_formulas %>%
    lapply("extract_formula_terms") %>% unlist() %>% unique() %>%
    reformulate(intercept = use_constant)

  return(combined_formula)
}

#' @keywords internal
remove_constant <- function(formula) {
  assert_formula(formula)
  replace_statement <- ~ . -1
  if (is_two_sided_formula(formula))
    replace_statement <- . ~ . - 1
  return(update(formula, replace_statement))
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
extract_transformed_vars <- function(formula,data,
                                     fix_contrasts = TRUE) {
  data <- data[0,]
  assert(data_permits_formula(formula,data),
         "The formula cannot be applied to the data!")
  factor_contrasts <- NULL
  if (fix_contrasts) factor_contrasts <- fixed_factor_contrasts(data,formula)

  tranformed_vars <-
    model.matrix(formula, data,
                 drop.unused.levels = FALSE,
                 contrasts.arg = factor_contrasts) %>%
    colnames()

  return(tranformed_vars)
}

#' @keywords internal
fixed_factor_contrasts <- function(data, formula = ~ .){
  assert_is(data,"data.frame")
  factor_cols <- which(sapply(data, is.factor,simplify = TRUE))
  factor_contrasts <- cols_keep(data,factor_cols) %>%
    lapply("contrasts", contrasts = FALSE)
  return(factor_contrasts)
}

#' @keywords internal
extract_formula_specials <- function(formula,specials) {

  # split all terms into special or general
  terms_obj_formula <- terms.formula(formula, specials, allowDotAsName = TRUE)
  all_terms <-  rownames(attr(terms_obj_formula, "factors"))

  special_terms <- attr(terms_obj_formula,"specials") %>%
    lapply(FUN = function(.s_index) {all_terms[.s_index]})
  non_special_terms <- setdiff(all_terms,unlist(special_terms))

  return(list("specials" = special_terms, "normals" = non_special_terms))
}

# ---- split the formula by "special" functions -------------------------------
#' @keywords internal
split_forumla_specials <- function(
  formula, specials) {
  assert_formula(formula)
  assert_is(specials,"character")

  split_terms <- extract_formula_specials(formula,specials)

  # first create the normal formula
  nt <- split_terms$normals
  normal_formula <- nt %|!0|% reformulate(nt,intercept = has_constant(formula))

  # then create the special formulas
  st <- split_terms$specials
  special_formulas <- st %|!|%
    plapply(special = names(st),string_formula = st,
            .f = "special_formula_as_rhs")

  null_special <- unlist(lapply(special_formulas,is.null))
  if (any(null_special)) {
    all_generals <- named_list(specials,normal_formula)
    special_formulas[null_special] <- all_generals[null_special]
  }


  return(special_formulas)
}

#' @keywords internal
special_formula_as_rhs <- function(special,string_formula) {
  if (length(string_formula) == 0) return(NULL)

  # make the special a function
  fun_env <- environment()
  assign(x = special, parse_fun_arguments_as_string, envir = fun_env)

  # evaluating the special -> returns its arguments a string
  special_rhs_formula <-
    eval_string_as_function(string_formula, e = fun_env) %>%
    reformulate_string()

  return(special_rhs_formula)
}

#' @keywords internal
reformulate_string <- function(string_formula) {
  reformulate(string_formula) %>% compact_formula()
}

#' @keywords internal
parse_fun_arguments_as_string <- function(x){
  deparse(substitute(x))
}

#' @keywords internal
eval_string_as_function <- function(x,e = environment()){
  eval(parse(text = x),envir = e)
}



