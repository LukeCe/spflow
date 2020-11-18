test_that("center: => correct output", {

  # vector case
  test_x <- 1:100
  actual <- center(test_x)
  expected <- test_x - mean(test_x)
  expect_equal(actual, expected)

  # data.frame case
  test_x <- cars
  actual <- center(test_x)
  expected <- test_x - colMeans(test_x)
  expect_equal(actual, expected)

  # matrix case
  test_x <- matrix(rnorm(300),3)
  actual <- center(test_x)
  expected <- test_x - colMeans(test_x)
  expect_equal(actual, expected)

})

test_that("vec_to_matrix: => correct output", {

  n_rows <- 10
  n_cols <- 10

  # sparse case
  test_vec <- 1:25
  test_mat <- matrix(0,n_rows,n_cols)
  test_mat[1:5,2:6] <- test_vec
  i_rows <- rep(1:5,5)
  j_cols <- rep(2:6,each = 5)
  complete <- length(test_vec) / length(test_mat)

  actual <- vec_to_matrix(test_vec, complete,n_rows,n_cols,i_rows,j_cols)
  expect_equivalent(as.matrix(actual),test_mat)

  # incomplete case
  test_vec <- 1:56
  test_mat <- matrix(0,n_rows,n_cols)
  test_mat[1:7,2:9] <- test_vec
  i_rows <- rep(1:7,8)
  j_cols <- rep(2:9,each = 7)
  complete <- length(test_vec) / length(test_mat)

  actual <- vec_to_matrix(test_vec, complete,n_rows,n_cols,i_rows,j_cols)
  expect_equal(actual,test_mat)

  # complete case
  test_vec <- 1:100
  test_mat <- matrix(test_vec,n_rows,n_cols)
  i_rows <- rep(1:10,10)
  j_cols <- rep(1:10,each = 10)
  complete <- 1
  actual <- vec_to_matrix(test_vec, complete,n_rows,n_cols,i_rows,j_cols)
  expect_equal(actual,test_mat)
})

test_that("fun: => correct output", {
  n_rows <- 2
  n_cols <- 2

  # incomplete (non-rectangular)
  test_vec <- 1:3
  test_mat <- matrix(c(1,0,2,3),n_rows,n_cols)
  i_rows <- c(1,1,2)
  j_cols <- c(1,2,2)
  complete <- length(test_vec) / length(test_mat)

  actual <- vec_to_matrix(test_vec, complete,n_rows,n_cols,i_rows,j_cols)
  expect_equal(actual,test_mat)
})



