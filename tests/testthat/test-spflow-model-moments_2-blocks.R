test_that("moment_empirical_var: symmetric case => correct output", {

  # test without weight
  test_model_matrices <- letters_model_matrices
  expected <- moments_letters

  # with weights
  actual_HH_wt <- moment_empirical_var(test_model_matrices,
                                       N = moments_letters$N,
                                       n_d = moments_letters$n_d,
                                       n_o = moments_letters$n_o)
  expect_equivalent(actual_HH_wt, moments_letters_wt$HH)

  # without weights
  test_model_matrices$wt <- NULL
  actual_HH <- moment_empirical_var(test_model_matrices,
                                    N = moments_letters$N,
                                    n_d = moments_letters$n_d,
                                    n_o = moments_letters$n_o)
  expect_equivalent(actual_HH, moments_letters$HH)

  })

test_that("moment_empirical_covar: symmetric case => correct output", {

  # test without weight
  test_model_matrices <- letters_model_matrices

  actual_HY <- lapply(test_model_matrices$Y_,
                      "moment_empirical_covar",
                      test_model_matrices) %>% lreduce("cbind")

  expect_equivalent(actual_HY, moments_letters$HY)
})

