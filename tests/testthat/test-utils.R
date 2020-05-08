# formulas --------------------------------------------------------------------
context("formulas")

test_that("Intercept removal works as intended", {

  # standard removal
  formula_original <- Sepal.Length ~ .
  formula_stripped <- Sepal.Length ~ . - 1
  expect_equal(
    object = model.matrix(formula_original %>% remove_intercept(),iris),
    expected = model.matrix(formula_stripped %>% pull_rhs(), iris))

  # double removal
  formula_original <- Sepal.Length ~ . - 1
  formula_stripped <- Sepal.Length ~ . - 1
  expect_equal(
    object = model.matrix(formula_original %>% remove_intercept(),iris),
    expected = model.matrix(formula_stripped %>% pull_rhs(), iris))

  # removal with special operator
  formula_original <- Sepal.Length ~ . + I(Sepal.Width + Petal.Length)
  formula_stripped <- Sepal.Length ~ . + I(Sepal.Width + Petal.Length) - 1
  expect_equal(
    object = model.matrix(formula_original %>% remove_intercept(),iris),
    expected = model.matrix(formula_stripped %>% pull_rhs(), iris))


  # removal with standard and special operator
  formula_original <-
    Sepal.Length ~ . + I(Sepal.Width + Petal.Length) + log(Petal.Length)
  formula_stripped <-
    Sepal.Length ~ . + I(Sepal.Width + Petal.Length) + log(Petal.Length) - 1
  expect_equal(
    object = model.matrix(formula_original %>% remove_intercept(),iris),
    expected = model.matrix(formula_stripped %>% pull_rhs(), iris))
})
