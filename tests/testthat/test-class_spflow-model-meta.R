
dummy_results <- data.frame(est = 1:4,row.names = LETTERS[1:4])
dummy_fitted <- 2:26
dummy_resid <- 1:25
dummy_N <- 256
dummy_ll <- 12
dummy_control <- list(estimation_method = "mle",
                      use_sdm = "TRUE",
                      model = "model 9")

class_dummy <- spflow_model_s4(
  ll = dummy_ll,
  estimation_results = dummy_results,
  estimation_control = dummy_control,
  N = dummy_N,
  sd_error = 2,
  R2_corr = 10,
  resid = dummy_resid,
  fitted = dummy_fitted,
  spatial_filter_matrix = matrix(1:16,4,4),
  design_matrix = NULL)


test_that("spflow_model: => correct construction", {

  expect_s4_class(class_dummy,"spflow_model_meta")
})


test_that("spflow_model: methods => correct output ", {

  # coef
  actual <- coef(class_dummy)
  expected <- lookup(dummy_results$est,rownames(dummy_results))
  expect_equal(actual,expected)

  # resid & fitted & nobs
  expect_equal(resid(class_dummy),dummy_resid)
  expect_equal(fitted(class_dummy),dummy_fitted)
  expect_equal(nobs(class_dummy),dummy_N)
  expect_equal(results(class_dummy),dummy_results)
})


test_that("spflow_model_mle: methods => correct output ", {

  # loglik
  expect_equal(logLik(class_dummy),dummy_ll)
})

