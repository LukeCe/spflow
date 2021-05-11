# ---- crossproduct_mat_list --------------------------------------------------
expect_equal({
  set.seed(1)
  n_test <- 5
  mat_list <- lapply(rep(n_test,5), function(.n) matrix(rnorm(.n^2),.n,.n))
  spflow:::crossproduct_mat_list(mat_list)
  },
  {
    set.seed(1)
    n_test <- 5
    mat <- Reduce("cbind", lapply(rep(n_test,5), function(.n) rnorm(.n^2)),
                  init = NULL)
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
  info = "for two matrix lists")

stop("depricated/testthat/test-utils-lin-alg.R")
