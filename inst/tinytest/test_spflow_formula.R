# ---- interpret_spflow_formula -------------------------------------------------

expect_equal({
  spflow:::interpret_spflow_formula(
    y ~ D_(a) + O_(b) + I_(c) + P_(d),
    list(use_intra = FALSE, sdm_variables = "none", estimation_method = "mle")
    )
  },
  {
    list(
      constants = list("global" = TRUE),
      norm      = list("Y_" = ~ y - 1, "D_" = ~ a - 1,
                       "O_" = ~ b - 1, "P_" = ~ d - 1)
    )
  },
  info = "formulas is split correctly (no sdm) (no intra)")

expect_equal({
  spflow:::interpret_spflow_formula(
    y ~ D_(a) + O_(b) + I_(c) + P_(d),
    list(use_intra = FALSE, estimation_method = "mle",
         sdm_variables = "same")
  )
  },
  {
    list(
      constants = list("global" = TRUE),
      norm      = list("Y_" = ~ y - 1, "D_" = ~ a - 1,
                       "O_" = ~ b - 1, "P_" = ~ d - 1),
      sdm       = list("D_" = ~ a - 1, "O_" = ~ b - 1)
      )
  },
  info = "formulas is split correctly (with sdm) (no intra)")

expect_equal({
    spflow:::interpret_spflow_formula(
      y ~ D_(a) + O_(b) + I_(c) + P_(d),
      list(use_intra = TRUE, estimation_method = "mle",
           sdm_variables = "same")
    )
  },
  {
    list(
      constants = list("global" = TRUE, "intra" = TRUE),
      norm      = list("Y_" = ~ y - 1, "D_" = ~ a - 1, "O_" = ~ b - 1,
                       "I_" = ~ c - 1, "P_" = ~ d - 1),
      sdm       = list("D_" = ~ a - 1, "O_" = ~ b - 1)
    )
  },
  info = "formulas is split correctly (with sdm) (with intra)")

expect_equal({
    spflow:::interpret_spflow_formula(
      y ~ D_(a) + O_(b) + I_(c) + P_(d),
      list(use_intra = TRUE,
           estimation_method = "s2sls",
           sdm_variables = "same",
           twosls_instrumental_variables = "same")
    )
  },
  {
    list(
      constants = list("global" = TRUE, "intra" = TRUE),
      norm      = list("Y_" = ~ y - 1, "D_" = ~ a - 1, "O_" = ~ b - 1,
                       "I_" = ~ c - 1, "P_" = ~ d - 1),
      sdm       = list("D_" = ~ a - 1, "O_" = ~ b - 1),
      inst      = list("D_" = ~ a - 1, "O_" = ~ b - 1,
                       "I_" = ~ c - 1, "P_" = ~ d - 1)
    )
  },
  info = "formulas is split correctly (with sdm, intra, instruments)")



expect_equal({
    spflow:::interpret_spflow_formula(
      y ~ . + log(a + 1)+ log(b + 1) + I_(c) + P_(d) - 1,
      list(use_intra = TRUE,
           estimation_method = "s2sls",
           sdm_variables = "same",
           twosls_instrumental_variables = "same")
    )
  },
  {
    list(
      constants = list("global" = FALSE, "intra" = TRUE),
      norm      = list("Y_" = ~ y - 1,
                       "D_" = ~ . + log(a + 1) + log(b + 1) - 1,
                       "O_" = ~ . + log(a + 1) + log(b + 1) - 1,
                       "I_" = ~ c - 1, "P_" = ~ d - 1),
      sdm       = list("D_" = ~ . + log(a + 1) + log(b + 1) - 1,
                       "O_" = ~ . + log(a + 1) + log(b + 1) - 1),
      inst      = list("D_" = ~ . + log(a + 1) + log(b + 1) - 1,
                       "O_" = ~ . + log(a + 1) + log(b + 1) - 1,
                       "I_" = ~ c - 1, "P_" = ~ d - 1)
    )
  },
  info = "use of dot shortcut for unspecifyed specials")


# ---- formula primitives -----------------------------------------------------
expect_true(spflow:::is_one_sided_formula( ~ 2))
expect_false(spflow:::is_one_sided_formula(NULL))
expect_false(spflow:::is_one_sided_formula(3 ~ 2))


expect_true(spflow:::is_two_sided_formula(3 ~ 2))
expect_false(spflow:::is_two_sided_formula(NULL))
expect_false(spflow:::is_two_sided_formula(~ 2))


expect_true(spflow:::formula_expands_factors( ~ ., iris))
expect_false(spflow:::formula_expands_factors( ~ ., cars))


expect_error(spflow:::has_constant(NULL),
             pattern ="^The input argument .* must be of class .*!")
expect_true(spflow:::has_constant(~ a + b))
expect_false(spflow:::has_constant(~ a + b - 1))


expect_error(spflow:::has_dot_shortcut(NULL),
             pattern = "^The input argument .* must be of class .*!")
expect_true(spflow:::has_dot_shortcut(y ~ b + .))
expect_false(spflow:::has_dot_shortcut(y ~ b + c))


expect_error(spflow:::data_permits_formula("a + a",data = cars))
expect_error(spflow:::data_permits_formula(~ a + b,data = letters))
expect_true(spflow:::data_permits_formula(~ . + 1, cars))
expect_true(spflow:::data_permits_formula(~ speed + dist, cars))
expect_false(spflow:::data_permits_formula(~ speed2 + dist2, cars))

# ---- compact_formula --------------------------------------------------------
expect_error(spflow:::compact_formula(NULL),
             info = "only works for formula")

expect_equal(spflow:::compact_formula( ~ a + a + b + b + c + c + c ),
             ~ a + b + c,
             info = "removes redundant variables")

expect_equal(spflow:::compact_formula( ~ a + 1 + 1 + 1),
             ~ a,
             info = "removes redundant intercepts")

expect_equal(spflow:::compact_formula( ~ a + 0 - 1),
             ~ a - 1,
             info = "cleans redundant intercept removal")

expect_equal(spflow:::compact_formula( ~ a + 0),
             ~ a - 1,
             info = "cleans intercept removal")

expect_equal(spflow:::compact_formula( ~ . + a + a - 1),
             ~ . + a - 1,
             info = "conserves the dot shortcut")

# ---- remove_constant --------------------------------------------------------
expect_error(spflow:::remove_constant(NULL))

expect_equal(spflow:::remove_constant(a ~ b), a ~ b - 1,
             info = "two sided formulas")

expect_equal(spflow:::remove_constant(~ b), ~ b - 1,
             info = "one sided formulas")

expect_equal(spflow:::remove_constant(~ b + 0), ~ b - 1,
             info = "double removal")

expect_equal(spflow:::remove_constant(~ . + b), ~ . + b - 1,
             info = "preserve the dot shortcut")

# ---- extract_formula_terms --------------------------------------------------
expect_equal(spflow:::extract_formula_terms(~ a + b + c),
             c("a","b","c"))

expect_equal(spflow:::extract_formula_terms(~ log(a) + sqrt(b) + exp(c)),
             c("log(a)","sqrt(b)","exp(c)"))

expect_equal(spflow:::extract_formula_terms(y ~ . + log(a)),
             c(".", "log(a)"))

expect_equal(spflow:::extract_formula_terms(y ~ .,data = cars),
             c("speed", "dist"),
             info = "expand dot when data is provided")

expect_error(spflow:::extract_formula_terms(1:10),
             pattern = "^The input argument .* must be of class .*!")

expect_error(spflow:::extract_formula_terms(a ~ b + c, data = 1:10),
             pattern = "^The input argument .* must be of class .*!")

# ---- combine_rhs_formulas ---------------------------------------------------
expect_error(spflow:::combine_rhs_formulas(NULL))

expect_equal(spflow:::combine_rhs_formulas(y ~ a + b, z ~ c + d),
             ~ a + b + c + d,
             info = "ignores lhs")

expect_equal(spflow:::combine_rhs_formulas(y ~ a + b -1, z ~ c + d),
             ~ a + b + c + d -1,
             info = "preserves constant")

expect_equal(spflow:::combine_rhs_formulas(y ~ a + b -1, z ~ c + d, NULL),
             ~ a + b + c + d -1)

expect_equal({
  spflow:::combine_rhs_formulas(
    y ~ log(abcdefg_abcdefg + 1) + log(abcdefg + 1),
    z ~ log(qwerty + 1) + log(qwerty_qwerty + 1) + log(qwerty_qwerty2 + 1))
},
{~ log(abcdefg_abcdefg + 1) + log(abcdefg + 1) +
    log(qwerty + 1) + log(qwerty_qwerty + 1) + log(qwerty_qwerty2 + 1)},
info = "preserves transformations")


# ---- extract_transformed_varnames -------------------------------------------
expect_equal({
  spflow:::extract_transformed_varnames(~ . , iris[,-5])$names
},
{
  c("(Intercept)", names(iris[,-5]))
},
info = "anticipates intercept creation")
expect_equal({
  spflow:::extract_transformed_varnames(~ . - 1, iris)$factors
},
{
  c("Speciesversicolor", "Speciesvirginica")
},
info = "anticipates factor expansions (no intercept)")
expect_equal({
  spflow:::extract_transformed_varnames(~ . - 1, iris)$names
},
{
  factor_expansion <- c("Speciesversicolor", "Speciesvirginica")
  c(colnames(iris)[-5],factor_expansion)
},
info = "anticipates factor expansions (no intercept)")
expect_equal({
  spflow:::extract_transformed_varnames(~ . , iris)$names
},
{
  factor_expansion <- c("Speciesversicolor", "Speciesvirginica")
  c("(Intercept)",colnames(iris)[-5],factor_expansion)
},
info = "anticipates factor expansions (with intercept)")

expect_equal({
  spflow:::extract_transformed_varnames(
    ~ . + log(speed) + log(dist), cars)$names
},
{c("(Intercept)", colnames(cars), "log(speed)", "log(dist)")
},
info = "anticipate dot expansion and preserver transformation")

expect_error(spflow:::extract_transformed_varnames(
  formula = NULL, data = iris))
expect_error(spflow:::extract_transformed_varnames(
  formula = ~ a + b, data = letters))
expect_error(spflow:::extract_transformed_varnames(
  formula = ~ not_available + Species, data = iris))

# ---- split_formula_specials -------------------------------------------------
expect_error(spflow:::split_formula_specials(NULL   ,NULL))
expect_error(spflow:::split_formula_specials(NULL   ,c("A_","B_")))
expect_error(spflow:::split_formula_specials(~ y - a,NULL))

expect_equal({
  spflow:::split_formula_specials(
    ~ D_(a) + O_(b) + I_(c) + P_(d),
    c("D_","O_","I_","P_"))
}, {
  list("D_" = ~ a, "O_" = ~ b, "I_" = ~ c, "P_" = ~ d)
},
info = "split into named list")

expect_equal(spflow:::split_formula_specials(~ .,c("D_","O_","I_","P_")),
             spflow:::named_list(c("D_","O_","I_","P_"),  ~ . ),
             info = "preserves dot shortcut")

expect_equal(spflow:::split_formula_specials(~ . - 1 ,c("D_","O_","I_","P_")),
             spflow:::named_list(c("D_","O_","I_","P_"),  ~ . - 1 ),
             info = "preserves intercept removal")

expect_equal({
  spflow:::split_formula_specials(~ . + a + b + c + P_(d),
                                  c("D_","O_","I_","P_"))
},
{
  c(spflow:::named_list( c("D_","O_","I_"),  ~ . + a + b + c ),
    list("P_" = ~ d))

},
info = "non specifyed formulas pick up from general case")

