# ---- cols_drop --------------------------------------------------------------
test_that("cols_drop: => correct output for drop by index", {

  test_df <- iris
  test_dt <- data.table::data.table(test_df)
  test_mat <- as.matrix(test_df)
  col_index <- 2:3

  actual_null <- cols_drop(NULL,col_index)
  expect_null(actual_null)

  # test dimensions + classes
  actual_all <- lapply(list(test_df,test_dt,test_mat), cols_drop, col_index)

  expected_dim <- dim(iris) - c(0,length(col_index))
  expect_correct_dim <- function(.actual) {
    expect_equal(dim(.actual), expected_dim)
    invisible(TRUE)
  }
  actual_all %>%
    lapply("expect_correct_dim")

  expected_classes <- c("data.frame","data.table","matrix")
  expect_correct_classes <- function(.actual, .expect) {
    expect_is(.actual, .expect)
    invisible(TRUE)
  }
  actual_all %>%
    plapply(.actual = .,
            .expect = expected_classes,
            .f = "expect_correct_classes")


})

test_that("cols_drop: => correct output for drop by names", {

  test_df <- iris
  test_dt <- data.table::data.table(test_df)
  test_mat <- as.matrix(test_df)
  col_index <- c("Petal.Length", "Petal.Width")

  # test dimensions + classes
  actual_all <- lapply(list(test_df,test_dt,test_mat), cols_drop, col_index)

  expected_dim <- dim(iris) - c(0,length(col_index))
  expect_correct_dim <- function(.actual) {
    expect_equal(dim(.actual), expected_dim)
    invisible(TRUE)
  }
  actual_all %>%
    lapply("expect_correct_dim")

  expected_classes <- c("data.frame","data.table","matrix")
  expect_correct_classes <- function(.actual, .expect) {
    expect_is(.actual, .expect)
    invisible(TRUE)
  }
  actual_all %>%
    plapply(.actual = .,
            .expect = expected_classes,
            .f = "expect_correct_classes")


})

test_that("cols_drop: => correct output for edge case of 1 column", {

  test_df <- cars
  test_dt <- data.table::data.table(test_df)
  test_mat <- as.matrix(test_df)
  col_index <- 1

  # test dimensions + classes
  actual_all <- lapply(list(test_df,test_dt,test_mat), cols_drop, col_index)

  expected_dim <- dim(cars) - c(0,length(col_index))
  expect_correct_dim <- function(.actual) {
    expect_equal(dim(.actual), expected_dim)
    invisible(TRUE)
  }
  actual_all %>%
    lapply("expect_correct_dim")

  expected_classes <- c("data.frame","data.table","matrix")
  expect_correct_classes <- function(.actual, .expect) {
    expect_is(.actual, .expect)
    invisible(TRUE)
  }
  actual_all %>%
    plapply(.actual = .,
            .expect = expected_classes,
            .f = "expect_correct_classes")


})

test_that("cols_drop: => correct output for unknows indexes", {

  test_df <- iris
  test_dt <- data.table::data.table(test_df)
  test_mat <- as.matrix(test_df)
  original_list <- list(test_df,test_dt,test_mat)
  expect_correctly_ignored <- function(.actual, .expected){
    expect_equal(.actual,.expected)
    return(invisible(TRUE))
  }

  # unknown numerical index
  col_index <- 12:13
  actual_all <- lapply(original_list, cols_drop, col_index)
  actual_all %>%
    plapply(.actual = ., .expected = original_list,
            .f = "expect_correctly_ignored")

  # unknown character index
  col_index <- c("aa","bb","cc")
  actual_all <- lapply(original_list, cols_drop, col_index)
  actual_all %>%
    plapply(.actual = ., .expected = original_list,
            .f = "expect_correctly_ignored")


})

test_that("cols_drop: => no side effects for data.table", {

  test_dt <- data.table::data.table(iris)
  test_dt_backup <- data.table::data.table(iris)

  col_index <- c("Sepal.Length", "Sepal.Width", "Petal.Length")
  dummy_actual <- test_dt %>% cols_drop(col_index)
  expect_equal(test_dt,test_dt_backup)

})

test_that("cols_drop: abusive input => error", {

  cols_drop <- 1:4
  vector <- LETTERS
  expect_error(cols_drop(vector,cols_drop),
               regexp = "^Function not implemented for objects of class.*")

})

# ---- cols_keep --------------------------------------------------------------
test_that("cols_keep: => correct output for keep by names", {

  test_df <- iris
  test_dt <- data.table::data.table(test_df)
  test_mat <- as.matrix(test_df)
  col_index <- c("Petal.Length", "Petal.Width")

  # test dimensions + classes
  actual_all <- lapply(list(test_df,test_dt,test_mat), cols_keep, col_index)

  expected_dim <- c(nrow(iris),2)
  expect_correct_dim <- function(.actual) {
    expect_equal(dim(.actual), expected_dim)
    invisible(TRUE)
  }
  actual_all %>%
    lapply("expect_correct_dim")

  expected_classes <- c("data.frame","data.table","matrix")
  expect_correct_classes <- function(.actual, .expect) {
    expect_is(.actual, .expect)
    invisible(TRUE)
  }
  actual_all %>%
    plapply(.actual = .,
            .expect = expected_classes,
            .f = "expect_correct_classes")


})


test_that("cols_keep: => correct output for keep by unknown names", {

  test_df <- iris
  test_dt <- data.table::data.table(test_df)
  test_mat <- as.matrix(test_df)
  col_index <- c("Petal.Length", "Petal.Width", "no_cols_have_this_name")

  # test dimensions + classes
  actual_all <- list(test_df,test_dt,test_mat) %>%
    lapply("cols_keep", cols_keep = col_index)

  expected_dim <- c(nrow(iris),2)
  expect_correct_dim <- function(.actual) {
    expect_equal(dim(.actual), expected_dim)
    invisible(TRUE)
  }
  actual_all %>%
    lapply("expect_correct_dim")

  expected_classes <- c("data.frame","data.table","matrix")
  expect_correct_classes <- function(.actual, .expect) {
    expect_is(.actual, .expect)
    invisible(TRUE)
  }
  actual_all %>%
    plapply(.actual = .,
            .expect = expected_classes,
            .f = "expect_correct_classes")


})

test_that("set_col_names: => correct output", {

  cars_copy <- data.table::copy(cars)
  test_df <- cars_copy
  test_dt <- cars_copy %>% data.table::setDT(.)
  test_mat <- as.matrix(cars_copy)

  new_names <- c("a","b")
  actual_df <- set_col_names(test_df,new_names)
  actual_dt <- set_col_names(test_dt,new_names)
  actual_mat <- set_col_names(test_mat,new_names)
  expect_equal(colnames(actual_df), new_names)
  expect_equal(colnames(actual_dt), new_names)
  expect_equal(colnames(actual_mat), new_names)
})

test_that("[prefix|suffix]_columns: => correct output", {

  cars_copy <- data.table::copy(cars)
  test_df <- cars_copy
  test_dt <- cars_copy %>% data.table::setDT(.)
  test_mat <- cars_copy %>% as.matrix()

  pre <- "A_"
  suff <- "_B"
  actual_df <- test_df %>% prefix_columns(pre) %>% suffix_columns(suff)
  actual_dt <- test_dt %>% prefix_columns(pre) %>% suffix_columns(suff)
  actual_mat <- test_mat %>% prefix_columns(pre) %>% suffix_columns(suff)

  expected_colnames <- pre %p% names(cars) %p% suff
  expect_equal(colnames(actual_df), expected_colnames)
  expect_equal(colnames(actual_dt), expected_colnames)
  expect_equal(colnames(actual_mat), expected_colnames)
})

