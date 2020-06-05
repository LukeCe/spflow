# formulas --------------------------------------------------------------------
context("utils - formulas")

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

test_that("Model matrix without contrasts works", {
  iris2 <- data.table::as.data.table(iris)
  iris2[ , id := 1:nrow(iris2)]
  data.table::setkey(iris2, id)
  test_model_matrix <- fix_contrast_model_matrix(
    ~ Petal.Length + Sepal.Length + Species - 1,
    iris2
  ) %>% as.data.frame()

  expect_length(test_model_matrix,2 + nlevels(iris$Species))

  test_model_matrix <- fix_contrast_model_matrix(
    ~ Petal.Length + Sepal.Length + Species - 1,
    iris2,
    .contrasts = 1
  ) %>% as.data.frame()

  expect_length(test_model_matrix,2 + (nlevels(iris$Species) - 1))


})

# FP style --------------------------------------------------------------------
context("utils - FP style")

test_that("List transposition works", {

  norm_list <- list("norm" = list("int" = 1, "par" = 1:2),
                    "sdm"  = list("par" = 1:3))

  norm_listT <- list("int" = list("norm" = 1),
                     "par" = list("norm" = 1:2, "sdm" = 1:3))

  expect_equal(object = spflow::translist(norm_list),
               expected = norm_listT)

})


