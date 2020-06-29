# ---- test extract_specials --------------------------------------------------
test_that("extract_specials: => correct output", {

  special_formulas <- c("D_","O_","I_","G_")

  # all specials
  input_formula <- ~ D_(a) + O_(b) + I_(c) + G_(d)
  actual <- extract_specials(input_formula,special_formulas)
  expected <- list("D_" = ~ a,
                   "O_" = ~ b,
                   "I_" = ~ c,
                   "G_" = ~ d) %>% lapply(remove_intercept)
  expect_equal(actual, expected)

  # all defaults
  input_formula <- ~ .
  actual <- extract_specials(input_formula,special_formulas)
  expected <- named_list(special_formulas,  ~ . - 1)
  expect_equal(actual, expected)

  # mix defaults and spcials
  input_formula <- ~ a + b + G_(c)
  actual <- extract_specials(input_formula,special_formulas)
  expected <- c(named_list(special_formulas[1:3], ~ a + b -1),
                list("G_" = ~ c - 1 ))
  expect_equal(actual, expected)
})

# ---- test split_flow_formula ------------------------------------------------
test_that("split_flow_formula: => correct output", {

  # no input
  actual_null <- split_flow_formula(NULL)
  expect_null(actual_null)

  # no string input
  actual_null <- split_flow_formula("NULL")
  expect_null(actual_null)

  # default formula
  formula_parts <- c("DX","OX","IX","G")
  formula_input <- Y ~ .
  actual <- split_flow_formula(formula_input)
  expected <- named_list(formula_parts, ~ . -1)
  expect_equal(actual, expected)


  # full formula
  formula_input <-
    Sepal.Length ~                           # 1
    D_(. +  I(Petal.Length + Petal.Width)) + # 2
    O_(. + log(Sepal.Width)) +               # 3
    I_(.) +                                  # 4
    G_(Species)                              # 5
  actual <- split_flow_formula(formula_input)
  expected <- list("DX" = ~. + I(Petal.Length + Petal.Width) - 1,
                   "OX" = ~. + log(Sepal.Width) - 1,
                   "IX" = ~. - 1,
                   "G"  = ~ Species - 1)
  expect_equal(actual, expected)
})

# ---- test interpret_flow_formula --------------------------------------------
test_that("interpret_flow_formula: s2sls => correct output", {

  # full formula all specials same sdm variables all instruments
  input_control_s2sls <- list(
    use_intra = TRUE,
    use_sdm = TRUE,
    sdm_variables = "same",
    instrumental_variables = "all",
    estimation_method = "s2sls")

  input_formula <- Y ~ D_(a) + O_(b) + I_(c) + G_(d)

  actual <- interpret_flow_formula(input_formula, input_control_s2sls)
  expected_norm <- list("Y"  = ~ Y,
                        "DX" = ~ a,
                        "OX" = ~ b,
                        "IX" = ~ c,
                        "G"  = ~ d) %>% lapply(remove_intercept)
  expect_equal(actual$norm, expected_norm)

  expect_sdm <- expected_norm[c("DX","OX","IX")]
  expect_equal(actual$sdm, expect_sdm)

  expect_inst <- named_list(c("DX","OX","IX","G"), ~ . - 1)
  expect_equal(actual$inst, expect_inst)

  # remove sdm and use special instruments
  input_control_s2sls$use_sdm <- FALSE
  input_control_s2sls$instrumental_variables <-
    ~ D_(a2) + O_(b2) + I_(c2) + G_(d2)

  actual <- interpret_flow_formula(input_formula, input_control_s2sls)
  expect_null(actual$sdm)

  expect_inst <- list("DX" = ~ a2 - 1,
                      "OX" = ~ b2 - 1,
                      "IX" = ~ c2 - 1,
                      "G"  = ~ d2 - 1)
  expect_equal(actual$inst, expect_inst)
})

test_that("interpret_flow_formula: mle => correct output", {

  ## full formula all specials all sdm variables all instruments
  input_control_mle <- list(
    use_intra = FALSE,
    use_sdm = TRUE,
    sdm_variables = "all",
    instrumental_variables = "same",
    estimation_method = "mle")
  input_formula <- Y ~ D_(a) + O_(b) + I_(c) + G_(d)

  # ... check that intra "I_()" is ignored
  actual <- interpret_flow_formula(input_formula, input_control_mle)
  expected_norm <- list("Y"  = ~ Y,
                        "DX" = ~ a,
                        "OX" = ~ b,
                        "G"  = ~ d) %>% lapply(remove_intercept)
  expect_equal(actual$norm, expected_norm)

  # ... check that "all keyword works for sdm"
  expect_sdm <- named_list(c("DX","OX"), ~ . - 1)
  expect_equal(actual$sdm, expect_sdm)

  # ... check that instruments are dropped
  expect_null(actual$inst)
})
