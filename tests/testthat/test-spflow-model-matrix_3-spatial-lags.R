test_that("var_usage_to_lag: for varnames and inst status => correct output", {

  advanced_usage <- list("norm" = c("X1","X2",     "X4"         ),
                         "sdm"  = c("X1",     "X3",     "X5"    ),
                         "inst" = c("X1","X2","X3",         "X6"))

  # Test varnames
  actual <- var_usage_to_lag(advanced_usage)
  actual <- lapply(actual,"sort")


  expect_lag3 <- c("X1",     "X3")
  expect_lag2 <- c("X1","X2","X3")
  expect_lag1 <- c("X1","X2","X3",     "X5","X6")
  expect_lag0 <- c("X1","X2",     "X4",     "X6")
  expect_equal(actual$lag3, expect_lag3)
  expect_equal(actual$lag2, expect_lag2)
  expect_equal(actual$lag1, expect_lag1)
  expect_equal(actual$lag0, expect_lag0)

  # Test inst status
  true_insts <- advanced_usage$inst
  expect_lag3 <- c("X1" = T, "X3" = T)
  expect_lag2 <- c("X1" = T ,"X2" = T, "X3" = T)
  expect_lag1 <- c("X1" = F, "X3" = F, "X5" = F, "X2" = T, "X6" = T)
  expect_lag0 <- c("X1" = F ,"X2" = F, "X4" = F, "X6" = T)
  actual <- var_usage_to_lag(advanced_usage,out_inst = TRUE)
  actual <- lapply(actual,"sort")
  expect_equal(actual$lag3, expect_lag3)
  expect_equal(actual$lag2, expect_lag2)
  expect_equal(actual$lag1, expect_lag1)
  expect_equal(actual$lag0, expect_lag0)
})

test_that("matrix_format: => correct output", {

  matrix_cols <- c("YY","GG")
  test_data <- pair_dat_LETTERS %>% cols_keep(matrix_cols) %>% as.matrix()
  args <- def_matrix_form_args(sp_multi_net_alphabet,"LETTERS_LETTERS")
  actual <- matrix_format(test_data,matrix_cols,args)
  expected_YY <- matrix(pair_dat_LETTERS$YY,10,10)
  expected_GG <- matrix(pair_dat_LETTERS$GG,10,10)
  expect_equal(actual$GG, expected_GG)
  expect_equal(actual$YY, expected_YY)
})

test_that("apply_matrix_od_lags: => correct output", {

  test_DW <- test_OW <- neighborhood(sp_net_letters)
  test_G <- matrix(pair_dat_letters$GG,nrow = nrow(test_DW))
  actual <- apply_matrix_od_lags(G = test_G,OW = test_OW,DW = test_DW,
                                      nb_lags = 2,name = "GG")
  # Lag is just multiplication by 4 in this case (look at OW and DW)
  expect_equal(test_G ,actual$GG)
  expect_equivalent(test_G*4 ,actual$GG.lag1)
  expect_equivalent(test_G*16 ,actual$GG.lag2)
})


test_that("orthoginolize_instruments: => correct output", {

  n <- 100
  var_a <- rnorm(n) # no inst
  var_b <- rnorm(n) # no inst
  var_c <- rnorm(n) # inst
  var_d <- rnorm(n) # inst

  # matrix with four instruments, two of them are redundant
  mat_with_inst <-
    cbind(var_a, var_b,
          var_c + 10 * var_a, var_c + 10 * var_b,
          var_d + 5 * var_b - 5* var_a, var_d - 5 * var_b + 5* var_a) %>%
    set_instrument_status(c(FALSE,FALSE,TRUE,TRUE,TRUE,TRUE))
  mat_without_inst <- cbind(var_a, var_b,var_c,var_d)

  actual <- orthoginolize_instruments(mat_with_inst)
  cor_expect <- cor(cbind(var_a, var_b,var_c,var_d))
  cor_actual <- cor(actual)
  # the redundant variables are filtered out and correlations are removed
  expect_equal(ncol(actual), 4)
  expect_true(all(abs(cor_expect) >= abs(cor_actual)))

  # when no instruments do nothing
  mat_without_inst <- cbind(var_a, var_b,var_c,var_d) %>%
    set_instrument_status(c(FALSE,FALSE,FALSE,FALSE))

  actual <- orthoginolize_instruments(mat_without_inst)
  expected <- mat_without_inst
  expect_equal(actual,expected)
})




