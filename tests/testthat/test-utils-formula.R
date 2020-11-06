test_that("is_[xxx]_sided_formula: => correct output", {

  # one sided
  expect_true((~ 2)     %>% is_one_sided_formula())
  expect_false(NULL     %>% is_one_sided_formula())
  expect_false( (3 ~ 2) %>% is_one_sided_formula())

  # two sided
  expect_true( (3 ~ 2) %>% is_two_sided_formula())
  expect_false(NULL    %>% is_two_sided_formula())
  expect_false( (~ 2)  %>% is_two_sided_formula())
})

test_that("compact_formula: => correct output", {

  # only works for formula
  expect_error(compact_formula(NULL))

  # redundant variables
  expected <- ~ a + b + c
  actual <- compact_formula( ~ a + a + b + b + c + c + c )
  expect_equal(actual, expected)

  # redundant intercept treatments => removal
  # + 0 - 1 ==> - 1
  expected <- ~ a - 1
  actual <- compact_formula( ~ a + 0 - 1)
  expect_equal(actual, expected)
  # + 0 ==> - 1
  actual <- compact_formula( ~ a + 0)
  expect_equal(actual, expected)

  # redundant intercept treatments => adding
  # + 1 + 1 + 1 ==>
  expected <- ~ a
  actual <- compact_formula( ~ a + 1 + 1 + 1)
  expect_equal(actual, expected)

  # keep dot shortcut and no intercept
  expected <- ~ . + a - 1
  actual <- compact_formula( ~ . + a + a - 1)
  expect_equal(actual, expected)
})

test_that("remove_constant: => correct output", {

  expect_error(remove_constant(NULL))

  # remove in 2 sided formula
  expected <- a ~ b - 1
  actual <- remove_constant(a ~ b)
  expect_equal(actual, expected)

  # remove in 1 sided formula
  expected <- ~ b - 1
  actual <- remove_constant(~ b)
  expect_equal(actual, expected)

  # remove in if already removed
  expected <- ~ b - 1
  actual <- remove_constant(~ b + 0)
  expect_equal(actual, expected)
})

test_that("has_constant: => correct output", {

  expect_error(has_constant(NULL),
               regexp = "^The input argument .* must be of class .*!")

  expect_true(has_constant(~ a + b))
  expect_false(has_constant(~ a + b + 0))
  expect_false(has_constant(~ a + b - 1))
})

test_that("has_dot_shortcut: => correct output", {

  expect_error(has_dot_shortcut(NULL),
               regexp = "^The input argument .* must be of class .*!")

  expect_true(has_dot_shortcut(y ~ b + .))
  expect_false(has_dot_shortcut(y ~ b + c))

})

test_that("extract_formula_terms: => correct output", {

  # normal terms
  actual <- extract_formula_terms(~ a + b + c)
  expected <- c("a","b","c")
  expect_equal(actual, expected)

  # transformed terms
  actual <- extract_formula_terms(~ log(a) + sqrt(b) + exp(c))
  expected <- c("log(a)","sqrt(b)","exp(c)")
  expect_equal(actual, expected)

  # dot terms
  actual <- extract_formula_terms(y ~ . + log(a))
  expected <- c(".", "log(a)")
  expect_equal(actual, expected)

  # dot terms expansion
  actual <- extract_formula_terms(y ~ .,data = cars)
  expected <- c("speed", "dist")
  expect_equal(actual, expected)
})

test_that("extract_formula_terms: abusive input => error", {

  expect_error(extract_formula_terms(1:10),
               regexp = "^The input argument .* must be of class .*!")
  expect_error(
    extract_formula_terms(a ~ b + c, data = 1:10),
    regexp = "^The input argument .* must be of class .*!")
})

test_that("data_permits_formula: => correct output", {

  expect_error(data_permits_formula("a + a",data = cars))
  expect_error(data_permits_formula(~ a + b,data = LETTERS))

  expect_true(data_permits_formula(~ . + 1, cars))
  expect_true(data_permits_formula(~ speed + dist, cars))
  expect_false(data_permits_formula(~ speed2 + dist2, cars))
})

test_that("combine_rhs_formulas: => correct output", {

  expect_error(combine_rhs_formulas(NULL))

  actual <- combine_rhs_formulas(y ~ a + b, z ~ c + d)
  expected <- ~ a + b + c + d
  expect_equal(actual, expected)

  actual <- combine_rhs_formulas(y ~ a + b -1, z ~ c + d)
  expected <- ~ a + b + c + d -1
  expect_equal(actual, expected)
})

test_that("extract_transformed_vars: factor variables =>  correct output", {

  factor_expansion <- c("Speciessetosa",
                        "Speciesversicolor",
                        "Speciesvirginica")

  actual <- extract_transformed_vars(~ Species - 1, iris)
  expected <- factor_expansion
  expect_equal(actual, expected)

  # with intercept
  actual <- extract_transformed_vars(~ Species, iris)
  expected <- c("(Intercept)", factor_expansion)
  expect_equal(actual, expected)
})

test_that("extract_transformed_vars: transformations =>  correct output", {

  transformed_vars <- c("log(speed)",
                        "log(dist)")

  actual <- extract_transformed_vars(~ . + log(speed) + log(dist), cars)
  expected <- c("(Intercept)", colnames(cars), transformed_vars)
  expect_equal(actual, expected)
})

test_that("extract_transformed_vars: abusive input => error", {

  expect_error(extract_transformed_vars(
    formula = NULL, data = iris))
  expect_error(extract_transformed_vars(
    formula = ~ a + b, data = letters))
  expect_error(extract_transformed_vars(
    formula = ~ not_available + Species, data = iris))

})

test_that("split_forumla_specials: => correct output", {

  # error cases
  expect_error(split_forumla_specials(NULL   ,NULL))
  expect_error(split_forumla_specials(NULL   ,c("A_","B_")))
  expect_error(split_forumla_specials(~ y - a,NULL))

  # define special formulas for test case
  special_formulas <- c("D_","O_","I_","G_")

  # test all specials
  input_formula <- ~ D_(a) + O_(b) + I_(c) + G_(d)
  actual <- split_forumla_specials(input_formula,special_formulas)
  expected <- list("D_" = ~ a, "O_" = ~ b, "I_" = ~ c, "G_" = ~ d)
  expect_equal(actual, expected)

  # all defaults
  input_formula <- ~ .
  actual <- split_forumla_specials(input_formula,special_formulas)
  expected <- named_list(special_formulas,  ~ . )
  expect_equal(actual, expected)

  # all defaults no intercept
  input_formula <- ~ . - 1
  actual <- split_forumla_specials(input_formula,special_formulas)
  expected <- named_list(special_formulas,  ~ . - 1)
  expect_equal(actual, expected)

  # missing specials: replaced by general
  input_formula <- ~ . + a + b + c + G_(d)
  actual <- split_forumla_specials(input_formula,special_formulas)
  expected <- named_list(special_formulas,  ~ . + a + b + c )
  expected$G_ <- ~ d
  expect_equal(actual, expected)
})
