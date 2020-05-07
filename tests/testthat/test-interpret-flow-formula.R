test_that("Expansion of explanatories works", {

  cases <- as.vector(define_case_prefixes())
  full_formulas <- named_list(cases, ~ .)

  test_formula <- ~ .
  expect_equal(
    object = expand_case_formulas(test_formula),
    expected = full_formulas)


  test_formula <- ~ . + O_(x + z)
  origin_formula <- full_formulas
  origin_formula$Orig_ <- ~ x + z
  expect_equal(
    object = expand_case_formulas(rhs_formula = test_formula),
    expected = origin_formula)

  test_formula <- ~ x + G_(z)
  mixed_formula <- named_list(cases, ~ x)
  mixed_formula$Pair_ <- ~ z

  expect_equal(
    object = expand_case_formulas(rhs_formula = test_formula),
    expected = mixed_formula)
})


test_that("Separation of lhs and rhs works", {

  cases <- as.vector(define_case_prefixes())
  test_formula <- y ~ .
  test_object <- expand_flow_formula(test_formula)

  expect_equal(object = test_object$interactions,
               expected = pull_lhs(test_formula))

  expect_equal(object = test_object[cases],
               expected = named_list(cases, ~.))

})


