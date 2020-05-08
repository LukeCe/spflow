# expand_case_formulas --------------------------------------------------------
context("expand_case_formulas")
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

# expand_main_formula ---------------------------------------------------------
context("expand_main_formula")
test_that("Separation of the five parts of the formula works", {

  initial_formula <-
    Sepal.Length ~                           # 1
    O_(. + log(Sepal.Width)) +               # 2
    D_(. +  I(Petal.Length + Petal.Width)) + # 3
    I_(.) +                                  # 4
    G_(Species)                              # 5

  forumula_case_split <- expand_main_formula(initial_formula)

  # Y
  expect_equal(object = forumula_case_split$interactions,
               expected = ~ Sepal.Length - 1)
  # O
  expect_equal(object = forumula_case_split$Orig_,
               expected = ~ . + log(Sepal.Width) - 1 )
  # D
  expect_equal(object = forumula_case_split$Dest_,
               expected =  ~ . +  I(Petal.Length + Petal.Width) - 1 )
  # I
  expect_equal(object = forumula_case_split$Intra_,
               expected =  ~ . - 1)
  # G
  expect_equal(object = forumula_case_split$Pair_,
               expected =  ~ Species - 1)
})

# expand_role_formula ---------------------------------------------------------
context("expand_role_formula")
initial_formula <-
  Sepal.Length ~                           # 1
  O_(. + log(Sepal.Width)) +               # 2
  D_(. +  I(Petal.Length + Petal.Width)) + # 3
  I_(.) +                                  # 4
  G_(Species)                              # 5

test_that("Expansion shortcuts work correctly", {

  case_names <- as.vector(define_case_prefixes())
  all_roles_template <- named_list(case_names,init = ~ . -1)
  null_roles_template <- named_list(case_names,init = ~ -1)

  expect_equal(
    object = {
      expand_role_formula(
        main_formula = initial_formula,
        role_formula = "all",
        use_pairs = TRUE,
        use_intra = TRUE)
      },
    expected = all_roles_template)

  expect_equal(
    object = {
      expand_role_formula(
        main_formula = initial_formula,
        role_formula = "none",
        use_pairs = TRUE,
        use_intra = TRUE)
    },
    expected = null_roles_template)


  # same roles...
  same_roles_object <- expand_role_formula(
    main_formula = initial_formula,
    role_formula = "same",
    use_pairs = TRUE,
    use_intra = TRUE)
  # O
  expect_equal(object = same_roles_object$Orig_,
               expected = ~ . + log(Sepal.Width) - 1 )
  # D
  expect_equal(object = same_roles_object$Dest_,
               expected =  ~ . +  I(Petal.Length + Petal.Width) - 1 )
  # I
  expect_equal(object = same_roles_object$Intra_,
               expected =  ~ . - 1)
  # G
  expect_equal(object = same_roles_object$Pair_,
               expected =  ~ Species - 1)
})

test_that("Explicit expansion works correctly", {

  explicit_roles <-
    ~ O_(Sepal.Width + Petal.Length) + D_(.) + I_(Species) + G_(Sepal.Width)

  explicit_roles_object <- expand_role_formula(
    main_formula = initial_formula,
    role_formula = explicit_roles,
    use_pairs = TRUE,
    use_intra = TRUE)
  # O
  expect_equal(object = explicit_roles_object$Orig_,
               expected = ~ Sepal.Width + Petal.Length - 1 )
  # D
  expect_equal(object = explicit_roles_object$Dest_,
               expected =  ~ . - 1 )
  # I
  expect_equal(object = explicit_roles_object$Intra_,
               expected =  ~ Species - 1)
  # G
  expect_equal(object = explicit_roles_object$Pair_,
               expected =  ~ Sepal.Width - 1)
})

test_that("Two sided formula is rejected in explicit expansion", {

  expect_error({
    expand_role_formula(
      main_formula = initial_formula,
      role_formula = initial_formula,
      use_pairs = TRUE,
      use_intra = TRUE
    )},
    "The declaration of variable roles musst be a one sided formula or*")
})

# expand_roles_and_cases ------------------------------------------------------
context("expand_roles_and_cases")

test_that("Expansion to all roles and cases works correctly", {

  initial_formula <-
    Sepal.Length ~                           # 1
    O_(. + log(Sepal.Width)) +               # 2
    D_(. +  I(Petal.Length + Petal.Width)) + # 3
    I_(.) +                                  # 4
    G_(Species)                              # 5

  formulas_by_case_and_roles <- expand_roles_and_cases(
    flow_forumula = initial_formula,
    flow_control = spflow_control(sdm_variables = "same",
                                  instrumental_variables = "same"),
    use_intra = TRUE)

  # list of lists
  # 3 roles -> [normal] [sdm] [instruments]
  # 5 cases -> [y] ~ [o] + [d] + [i] + [g]
  expect_length(formulas_by_case_and_roles,3L)
  expect_length(formulas_by_case_and_roles[[1]], 5L)

  # no [y] for the sdm and instruments
  expect_length(formulas_by_case_and_roles[[2]], 4L)
  expect_length(formulas_by_case_and_roles[[3]], 4L)

  # Normal and instrument cases should be the same
  # Normal and sdm should be the same (without G )
  case_names <- define_case_prefixes() %>% as.vector()
  normal_case_without_y <-
    formulas_by_case_and_roles[["normal_variables"]][case_names]

  normal_case_without_y_and_g <- normal_case_without_y
  normal_case_without_y_and_g$Pair_ <- ~ - 1

  expect_equal(object = formulas_by_case_and_roles$sdm_variables,
               expected = normal_case_without_y_and_g)

  expect_equal(object = formulas_by_case_and_roles$instrumental_variables,
               expected = normal_case_without_y)
})


