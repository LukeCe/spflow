test_that("crossproduct_mat_list: => correct output", {

  n_test <- 10
  mat_list_1 <- lapply(rep(n_test,5), function(.n) matrix(rnorm(.n^2),.n,.n))
  vec_format_1 <- lapply(mat_list_1, as.vector) %>% lreduce(cbind)

  mat_list_2 <- lapply(rep(n_test,5), function(.n) matrix(rnorm(.n^2),.n,.n))
  vec_format_2 <- lapply(mat_list_2, as.vector) %>% lreduce(cbind)

  # test single input
  actual <- crossproduct_mat_list(mat_list_1)
  expected <- crossprod(vec_format_1)
  expect_equivalent(actual, expected)

  # double input case
  actual <- crossproduct_mat_list(mat_list_1, mat_list_2)
  expected <- crossprod(vec_format_1,vec_format_2)
  expect_equivalent(actual, expected)

  # forced symmetry
  actual <- crossproduct_mat_list(mat_list_1, mat_list_2, force_sym = TRUE)
  select_upper <- upper.tri(actual,diag = TRUE)
  expected_upper <- crossprod(vec_format_1,vec_format_2) %[% select_upper
  expect_equivalent(actual[select_upper]   , expected_upper)
  expect_equivalent(t(actual)[select_upper], expected_upper)
})



test_that("impose_orthogonality: => correct output", {

  actual_null <- impose_orthogonality(NULL,NULL)
  expect_null(actual_null)

  n <- 100
  ran_a <- rnorm(n)
  ran_b <- rnorm(n)
  ran_c <- rnorm(n)
  ran_10ac <- 10 * ran_a + ran_c

  mat_abc <- cbind(ran_a,ran_b,ran_10ac)
  corr_pre <- cor(mat_abc)
  actual_cor <- cor(impose_orthogonality(mat_abc,column_sets = list(c(1,2),3)))
  expected_cor22 <- corr_pre[1:2,1:2]
  expect_equal(actual_cor[1:2,1:2], expected_cor22)
  # compare previous < current correlation
  # cor(a,c) < cor(a,b)
  expect_lt(abs(actual_cor[1,3]), abs(corr_pre[1,2]))
  # cor(a,b) < cor(a,b)
  expect_lt(abs(actual_cor[2,3]), abs(corr_pre[2,3]))
})


test_that("linear_dim_reduction: => correct output", {

  rows <- 450
  cols <- 5
  # all but one columns of the additional columns are uninformative ...
  test_matrix <- matrix(rnorm(rows * cols),ncol = cols)
  test_matrix <- cbind(test_matrix, test_matrix + rnorm(rows))

  actual <- linear_dim_reduction(test_matrix,var_threshold = 1)
  expected_dim <- c(rows,cols + 1)
  expect_equal(dim(actual), expected_dim)
})

test_that("sandwich_prod: => correct output", {

  actual_null <- sandwich_prod(w1 = NULL,w2 = NULL,mat = NULL)
  expect_null(actual_null)

  n <- 10
  test_w <- diag(n)*2
  test_mat <- matrix(rnorm(n^2),n,n)

  # both sides
  actual <- sandwich_prod(test_w,test_mat,test_w)
  expected <- test_mat*4
  expect_equal(actual, expected)

  # only left
  actual <- sandwich_prod(test_w,test_mat,NULL)
  expected <- test_mat*2
  expect_equal(actual, expected)

  # only right
  actual <- sandwich_prod(test_w,test_mat,NULL)
  expected <- test_mat*2
  expect_equal(actual, expected)

  # none right
  actual <- sandwich_prod(NULL,test_mat,NULL)
  expected <- test_mat
  expect_equal(actual, expected)
})

