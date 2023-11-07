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

# ---- rbind_fill_left---------------------------------------------------------
expect_equal({
  spflow:::rbind_fill_left(matrix(1:6,nrow = 2),
                           matrix(1:4,nrow = 2),
                           matrix(1:2,nrow = 2))
},
{
  rbind(matrix(1:6,nrow = 2),
        cbind(c(NA,NA),matrix(1:4,nrow = 2)),
        cbind(c(NA,NA),c(NA,NA),matrix(1:2,nrow = 2)))
})

expect_equal({
  spflow:::rbind_fill_left(matrix(1:6,nrow = 2),
                           matrix(1:4,nrow = 2),
                           matrix(1:2,nrow = 2),fill = 0)
},
{
  rbind(matrix(1:6,nrow = 2),
        cbind(c(0,0),matrix(1:4,nrow = 2)),
        cbind(c(0,0),c(0,0),matrix(1:2,nrow = 2)))
})

# ---- stack_columns ----------------------------------------------------------
expect_equal({
  test_mat <- matrix(letters[1:4],2,2,
                     dimnames = list(LETTERS[1:2],LETTERS[3:4]))
  spflow:::stack_columns(test_mat)
},
{
  data.frame(col = factor(c("C","D","C","D")),
             row = factor(c("A","A","B","B")),
             value = letters[1:4])
})

# ---- trace_sequence ---------------------------------------------------------
expect_equal(spflow:::trace_sequence(diag(3),max_power = 5), rep(3,5))


# ---- count_pattern  ---------------------------------------------------------
expect_equal(spflow:::count_pattern(c("aab","bbc", "acc"), "b"),
             c(1,2,0))

# ---- update_logicals --------------------------------------------------------
expect_equal(spflow:::update_logicals(c(T,T),T,F,c(T,F)),
             c(F,F))

expect_equal(spflow:::update_logicals(c(T,T),T,NULL,c(T,F)),
             c(T,F))

expect_equal(spflow:::update_logicals(c(T,T),T,F,c(T,F), by = "|"),
             c(T,T))

# ---- flatlist ---------------------------------------------------------------
nlist <- list(
  "A" = "A",
  "B" = list("B" = "B", "C" = "C"),
  "D" = list("D" = list("D" = "D"), "E" = "E", "F" = list("F" = list("F" = "F"))))

expect_equal(spflow:::flatlist(nlist), as.list(LETTERS[1:6]),
             check.attributes = FALSE)

# ---- nb2Mat -----------------------------------------------------------------
expect_true({
  nb <- spdep::poly2nb(germany_grid)
  W <- spdep::nb2mat(nb, style = "B")
  all(Matrix::Matrix(W) == nb2Mat(nb))
}, info = "Check in compact spatial data")


expect_true({
  geusa <- rbind(germany_grid,usa_grid)
  nb <- spdep::poly2nb(geusa)
  W <- spdep::nb2mat(nb, style = "B", zero.policy = TRUE)
  all(Matrix::Matrix(W) == nb2Mat(nb))
}, info = "Check for spatial data with holes.")
