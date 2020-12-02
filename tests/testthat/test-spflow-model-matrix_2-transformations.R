test_that("by_source_model_matrix: => correct output", {

  ## Define the test case based on the alphabet test data...
  #... formulas
  test_formula <- exp(YY) ~ I(XX^2) + G_(exp(GG))
  part_variables <-
    c("Y_" = "exp(YY)", "G_" = "exp(GG)", lookup("I(XX^2)", c("O_","D_","I_")))
  var_to_form <- function(v) {reformulate_string(v) %>% remove_constant()}
  part_formulas <- part_variables %>% lapply("var_to_form")
  part_formulas <- list("norm" = part_formulas,
                        "sdm" = part_formulas[c("O_","D_","I_")],
                        "inst" = part_formulas[c("O_","D_","I_","G_")])
  #... data
  data_sources <- list("orig" = net_dat_letters %>% cols_keep("XX"),
                       "pair" = pair_dat_letters %>% cols_keep(c("YY","GG")))

  #... tests
  actual <- by_source_model_matrix(part_formulas, data_sources)
  expect_orig <- model.matrix( ~ I(XX^2) -1 ,data = net_dat_letters)
  expect_pair <- model.matrix( ~ exp(YY) + exp(GG) -1 ,data = pair_dat_letters)
  expect_equal(actual$orig,expect_orig, check.attributes = FALSE)
  expect_equal(actual$pair,expect_pair, check.attributes = FALSE )
})


test_that("combine_formulas_by_source: => correct output", {

  formula_parts <- list("Y_" = ~ y, "G_" =  ~ dist,
                        "D_" = ~ a + b, "O_" = ~ b + c, "I_" = ~ d) %>%
    lapply("remove_constant")

  # test case 1 (orig == dest)
  actual <- combine_formulas_by_source(c("pair","orig"),formula_parts)
  expected <- list("pair"= ~ y + dist - 1,
                   "orig" = ~ a + b + c + d - 1)
  expect_equal(actual, expected)

  # test case 1 (orig != dest)
  actual <- combine_formulas_by_source(c("pair","orig","dest"),formula_parts)
  expected <- list("pair" = ~ y + dist - 1,
                   "dest" = ~ a + b - 1,
                   "orig" = ~ b + c - 1)
  expect_equal(actual, expected)
})

test_that("validate_source_formulas: => correct errors", {

  test_data <- cars
  test_formula <- ~ speed + dist + not_avilable -1

  expect_error(
    validate_source_formulas(test_formula, test_data, "orig"),
    regexp = "^The variables \\[not_avilable\\] were not found in the data .*")
})

test_that("flow_conform_model_matrix: => correct output", {

  ### Zero factors
  # test 1: with intercept
  expected <- model.matrix(~ . , cars) %>% cols_drop("(Intercept)")
  actual <- flow_conform_model_matrix(~ . , cars)
  expect_equal(actual, expected)

  # test 2: no intercept
  expected <- model.matrix(~ . , cars) %>% cols_drop("(Intercept)")
  actual <- flow_conform_model_matrix(~ . - 1 , cars)
  expect_equal(actual, expected)

  ### One factor
  # test 1: with intercept
  expected <- model.matrix(~ . -1, iris)
  actual <- flow_conform_model_matrix(~ . , iris)
  expect_equal(actual, expected)

  # test 2: no intercept
  expected <- model.matrix(~ . -1, iris)
  actual <- flow_conform_model_matrix(~ . - 1, iris)
  expect_equal(actual, expected)

  ### Two factors
  dat_two_fact <- as.data.frame(ChickWeight)

  # test 1: with intercept
  expected <- model.matrix(~ . -1, dat_two_fact)
  actual <- flow_conform_model_matrix(~ ., dat_two_fact)
  expect_equal(actual, expected)

  # test 2: no intercept
  expected <- model.matrix(~ . -1, dat_two_fact)
  actual <- flow_conform_model_matrix(~ . - 1, dat_two_fact)
  expect_equal(actual, expected)

})

test_that("flow_conform_model_matrix: with centering => correct output", {

  ### Zero factors
  # test 1: with intercept
  expected <- model.matrix(~ center(speed) + center(dist) -1 , cars)
  actual <- flow_conform_model_matrix(~ center(speed) + center(dist) , cars)
  expect_equal(actual, expected, check.attributes = FALSE)

  expected <- model.matrix(~ center(log(speed)) + center(log(dist)) -1 , cars)
  actual <- flow_conform_model_matrix(~ center(log(speed)) + center(log(dist)),
                                      cars)
  expect_equal(actual, expected, check.attributes = FALSE)

})
