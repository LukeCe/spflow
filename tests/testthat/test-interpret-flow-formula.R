test_that("interpret_flow_formula: case 1 => correct output", {

  # test case 1 without intra and sdm
  flow_formula <- y ~ D_(a) + O_(b) + I_(c) + G_(d)
  flow_control <- list(use_intra = FALSE,
                       use_sdm = FALSE,
                       estimation_method = "mle")

  actual <- interpret_flow_formula(flow_formula,flow_control)
  expected_const <- list(global = TRUE)
  expected_norm <- list("Y_" = ~ y, "D_" = ~ a , "O_" = ~ b,  "G_" = ~ d)
  expect_equal(actual$const, expected_const)
  expect_equal(actual$norm, expected_norm)
  expect_null(actual$sdm)
  expect_null(actual$inst)
})

test_that("interpret_flow_formula: case 2 => correct output", {

  ## test case 2 with intra (no const) and sdm
  # same shortcut
  flow_formula <- y ~ D_(a) + O_(b) + I_(c - 1) + G_(d)
  flow_control <- list(use_intra = TRUE,
                       use_sdm = TRUE,
                       sdm_variables = "same",
                       estimation_method = "mle")

  actual <- interpret_flow_formula(flow_formula,flow_control)
  expected_const <- list(global = TRUE, intra = FALSE)
  expected_norm <-
    list("Y_" = ~ y, "D_" = ~ a , "O_" = ~ b, "I_" = ~ c - 1, "G_" = ~ d)
  expected_sdm <- expected_norm[c("D_","O_","I_")] # no Y_ and no G_ for sdm
  expect_equal(actual$const, expected_const)
  expect_equal(actual$norm, expected_norm)
  expect_equal(actual$sdm, expected_sdm)
  expect_null(actual$inst)
})

test_that("interpret_flow_formula: case 3 => correct output", {

  ## test case 3 with instruments intra and sdm and no global const
  # gen formula part as shortcut for O_ and D_
  flow_formula <- y ~ a + b - 1 + I_(c) + G_(d)

  # shortcut: "same" for sdm ; "all" for inst
  flow_control <- list(use_intra = TRUE,
                       use_sdm = TRUE,
                       sdm_variables = "same",
                       instrumental_variables = "all",
                       estimation_method = "s2sls")

  actual <- interpret_flow_formula(flow_formula,flow_control)
  expected_const <- list(global = FALSE, intra = TRUE)
  expected_norm <-
    list("Y_" = ~ y,
         "D_" = ~ a + b - 1, "O_" = ~ a + b - 1, "I_" = ~ c,
         "G_" = ~ d)
  expected_sdm <- expected_norm[c("D_","O_","I_")] # no Y_ and no G_ for sdm
  expected_inst <- named_list(c("D_","O_","I_","G_"), ~ .) # no Y_ and for inst
  expect_equal(actual$const, expected_const)
  expect_equal(actual$norm, expected_norm)
  expect_equal(actual$sdm, expected_sdm)
  expect_equal(actual$inst, expected_inst)
})

test_that("interpret_flow_formula: case 4 => correct output", {

  ## test case 3 with . shortcut
  # gen formula part as shortcut for O_ and D_
  flow_formula <- y ~ . + a + b + I_(c) + G_(d) - 1

  # shortcut: "same" for sdm ; "all" for inst
  flow_control <- list(use_intra = TRUE,
                       use_sdm = TRUE,
                       sdm_variables = "same",
                       instrumental_variables = "all",
                       estimation_method = "s2sls")

  actual <- interpret_flow_formula(flow_formula,flow_control)
  expected_const <- list(global = FALSE, intra = TRUE)
  expected_norm <- list("Y_" = ~ y,
                        "D_" = ~ . + a + b - 1,
                        "O_" = ~ . + a + b - 1,
                        "I_" = ~ c,
                        "G_" = ~ d)
  expected_sdm <- expected_norm[c("D_","O_","I_")] # no Y_ and no G_ for sdm
  expected_inst <- named_list(c("D_","O_","I_","G_"), ~ .) # no Y_ and for inst
  expect_equal(actual$const, expected_const)
  expect_equal(actual$norm, expected_norm)
  expect_equal(actual$sdm, expected_sdm)
  expect_equal(actual$inst, expected_inst)
})
