expand_flow_formula <- function(f) {

  role_prefixes <- c("O_"  = "Orig_",
                     "D_"  = "Dest_",
                     "I_"  = "Intra_",
                     "G_" = "Pair_")

  role_specials <- names(role_prefixes)

  terms_formula <- terms.formula(f, specials = role_specials)
  terms_labs <- labels(terms_formula)
  terms_specials <- attr(terms_formula,"specials") %>% compact()

  if (length(terms_specials) == 0) {
    role_formulas <- named_list(role_prefixes,f[-2])
  }

  dependent_vars <- list("interactions" = f[1:2])

  return(c(role_formulas,dependent_vars))
}


prefix_formula_vars <- function(f, prefix = "O_") {

  a <- "b"
  formula_expression <- substitute(f)
  vars <- unique(terms(formula_expression))
  match_vars <- "\\b" %p% vars %p% "\\b"
  replace_vars <- prefix %p% vars

  prefixed_formula <- deparse(formula_expression)

  for (i in seq_along(match_vars)) {
    prefixed_formula <-
      gsub(pattern = match_vars[i],
           replacement = replace_vars[i],
           prefixed_formula)
  }

  str2lang(prefixed_formula)
}

O_ <- function(f) {
  prefix_formula_vars(f,"Orig_")
}

D_ <- function(f) {
  prefix_formula_vars(f,"Dest_")
}

I_ <- function(f) {
  prefix_formula_vars(f,"Intra_")
}

