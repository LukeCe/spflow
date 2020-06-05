# ---- setup ------------------------------------------------------------------
test_model_matrices <- stop("TODO")

vec_ref_Y
vec_ref_Z

test_that("spflow_model_moments_mat: s2sls => correct output", {

  actual <- spflow_model_moments_mat(fun_input)
  expected <- reference
  expect_equal(actual, expected)
})

