# ---- crossproduct_mat_list --------------------------------------------------
expect_equal({
  set.seed(1)
  n_test <- 5
  mat_list <- lapply(rep(n_test,5), function(.n) matrix(rnorm(.n^2),.n,.n))
  names(mat_list) <- LETTERS[seq_len(n_test)]
  spflow:::crossproduct_mat_list(mat_list)
  },
  {
    set.seed(1)
    n_test <- 5
    mat <- Reduce("cbind", lapply(rep(n_test,5), function(.n) rnorm(.n^2)),
                  init = NULL)
    colnames(mat) <- LETTERS[seq_len(n_test)]
    crossprod(mat)
  },
  info = "for a single matrix list")

expect_equal({
  set.seed(1)
  n_test <- 5
  n2_test <- 5
  mat_list <- lapply(rep(n_test,5), function(.n) matrix(rnorm(.n^2),.n,.n))
  mat_list2 <- lapply(rep(n2_test,5), function(.n) matrix(rnorm(.n^2),.n,.n))
  spflow:::crossproduct_mat_list(mat_list, mat_list2,force_sym = FALSE)
  },
  {
    set.seed(1)
    n_test <- 5
    n2_test <- 5
    mat <- Reduce("cbind", lapply(rep(n_test,5), function(.n) rnorm(.n^2)),
                  init = NULL)
    mat2 <- Reduce("cbind", lapply(rep(n2_test,5), function(.n) rnorm(.n^2)),
                  init = NULL)
    crossprod(mat,mat2)
  },
  info = "for two matrix lists",
  tolerance = sqrt(.Machine$double.eps))


# ---- impose_orthogonality ---------------------------------------------------
expect_null(spflow:::impose_orthogonality(NULL,NULL))

expect_true({
  n <- 100
  ran_a <- rnorm(n)
  ran_b <- rnorm(n)
  ran_c <- rnorm(n)
  ran_10ac <- 10 * ran_a + ran_c

  mat_abc <- cbind(ran_a,ran_b,ran_10ac)
  corr_pre <- cor(mat_abc)
  actual_cor <- cor(spflow:::impose_orthogonality(
    mat_abc,column_sets = list(c(1,2),3)))
  expected_cor22 <- corr_pre[1:2,1:2]

  all(c(
    # first two columns dont change
    actual_cor[1:2,1:2] == corr_pre[1:2,1:2],
    # third column has reduced correlation with first
    abs(actual_cor[1,3]) < abs(corr_pre[1,3]),
    # correlation afterwards is less then for random vectors
    abs(actual_cor[1,3]) < abs(corr_pre[2,3])))
  },
  info = "correlation after orthogonal projection is reduced")

# ---- linear_dim_reduction ---------------------------------------------------
expect_equal({
  rows <- 450
  cols <- 5
  # all but one column of the additional columns are uninformative ...
  test_matrix <- matrix(rnorm(rows * cols),ncol = cols)
  test_matrix <- cbind(test_matrix, test_matrix + rnorm(rows))

  dim(spflow:::linear_dim_reduction(test_matrix,var_threshold = 1))
  },
  {
    c(450,6)},
  info = "uninformative columns are dropped")

# ---- sandwich_prod ----------------------------------------------------------
expect_null(spflow:::sandwich_prod(w1 = NULL,w2 = NULL,mat = NULL))
expect_equal(spflow:::sandwich_prod(diag(2,5,5),matrix(1,5,5),diag(2,5,5)),
             matrix(4,5,5))
expect_equal(spflow:::sandwich_prod(diag(2,5,5),matrix(1,5,5),NULL),
             matrix(2,5,5))
expect_equal(spflow:::sandwich_prod(NULL,matrix(1,5,5),diag(2,5,5)),
             matrix(2,5,5))
expect_equal(spflow:::sandwich_prod(NULL,matrix(1,5,5),NULL),
             matrix(1,5,5))
