test_that("split_flow_formula: => correct output", {

  # no input
  actual_null <- split_flow_formula(NULL)
  expect_null(actual_null)

  # no string input
  actual_null <- split_flow_formula("NULL")
  expect_null(actual_null)

  # default formula
  formula_input <- Y ~ .
  actual <- split_flow_formula(formula_input)
  expected <- reference
  expect_equal(actual, expected)


  # full formula
  formula_input <-
    Sepal.Length ~                           # 1
    O_(. + log(Sepal.Width)) +               # 2
    D_(. +  I(Petal.Length + Petal.Width)) + # 3
    I_(.) +                                  # 4
    G_(Species)                              # 5
  actual <- split_flow_formula(formula_input)
  expected <- reference
  expect_equal(actual, expected)


})

