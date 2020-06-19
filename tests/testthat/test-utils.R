# formulas --------------------------------------------------------------------
test_that("remove_intercept: => correct output", {

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

test_that("fix_contrast_model_matrix: data.table => correct output", {

  iris2 <- data.table::as.data.table(iris)
  test_model_matrix <- fix_contrast_model_matrix(
    ~ Petal.Length + Sepal.Length + Species,
    iris2
  ) %>% as.data.frame()

  nb_normal_vars <- 2
  nb_dummies_from_factor <- nlevels(iris$Species)
  add_intercept <- 1
  expected_len <- nb_normal_vars + nb_dummies_from_factor + add_intercept
  expect_length(test_model_matrix, expected_len)


  test_model_matrix <- fix_contrast_model_matrix(
    ~ Petal.Length + Sepal.Length + Species - 1,
    iris2) %>% as.data.frame()
  expected_len <- nb_normal_vars + nb_dummies_from_factor
  expect_length(test_model_matrix, expected_len)
})

test_that("fix_contrast_model_matrix: data.frame => correct output", {

  iris2 <- iris
  test_model_matrix <- fix_contrast_model_matrix(
    ~ Petal.Length + Sepal.Length + Species,
    iris2
  ) %>% as.data.frame()

  nb_normal_vars <- 2
  nb_dummies_from_factor <- nlevels(iris$Species)
  add_intercept <- 1
  expected_len <- nb_normal_vars + nb_dummies_from_factor + add_intercept
  expect_length(test_model_matrix, expected_len)


  test_model_matrix <- fix_contrast_model_matrix(
    ~ Petal.Length + Sepal.Length + Species - 1,
    iris2) %>% as.data.frame()
  expected_len <- nb_normal_vars + nb_dummies_from_factor
  expect_length(test_model_matrix, expected_len)
})
# FP style --------------------------------------------------------------------
test_that("translist: => correct output", {

  norm_list <- list("norm" = list("int" = 1, "par" = 1:2),
                    "sdm"  = list("par" = 1:3))

  norm_listT <- list("int" = list("norm" = 1),
                     "par" = list("norm" = 1:2, "sdm" = 1:3))

  expect_equal(object = translist(norm_list),
               expected = norm_listT)

})


