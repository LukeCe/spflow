test_that("Expansion of explanatories works", {

  cases <- as.vector(define_case_prefixes())
  full_formulas <- named_list(cases, remove_intercept(~ . ))

  test_formula <- ~ .
  expect_equal(
    object = expand_case_formulas(test_formula),
    expected = full_formulas)


  test_formula <- ~ . + O_(x + z)
  origin_formula <- full_formulas
  origin_formula$Orig_ <- remove_intercept(~ x + z)
  expect_equal(
    object = expand_case_formulas(rhs_formula = test_formula),
    expected = origin_formula)

  test_formula <- ~ x + G_(z)
  mixed_formula <- named_list(cases, remove_intercept( ~ x))
  mixed_formula$Pair_ <- remove_intercept(~ z)

  expect_equal(
    object = expand_case_formulas(rhs_formula = test_formula),
    expected = mixed_formula)
})

test_that("Formula interface contiues to work", {
  test_formula <- Sepal.Length ~ I(Sepal.Width + Petal.Length) + Species
  target_matrix <- model.matrix(remove_intercept(test_formula),iris)

  test_objects <- expand_case_formulas(rhs_formula = pull_rhs(test_formula))

  lapply(test_objects, function(.tf)
    expect_equal(object = model.matrix(.tf,iris),
                 expected = target_matrix))



})

test_that("Separation of lhs and rhs works", {

  cases <- as.vector(define_case_prefixes())
  test_formula <- y ~ .
  test_object <- expand_flow_formula(test_formula)

  expect_equal(object = test_object$interactions,
               expected = test_formula %>% pull_lhs())

  expect_equal(object = test_object[cases],
               expected = named_list(cases, remove_intercept(~.)))
})


