test_that("by_source_model_matrix: => correct output", {

  ## Define the test case based on the alphabet test data...
  #... formulas
  test_formula <- exp(YY) ~ I(XX^2) + G_(exp(GG))
  part_variables <-
    c("Y_" = "exp(YY)", "G_" = "exp(GG)", lookup("I(XX^2)", c("O_","D_","I_")))
  var_to_form <- function(v) {reformulate_string(v) %>% remove_constant()}
  part_formulas <- part_variables %>% lapply("var_to_form")
  part_formulas <- list("norm" = part_formulas,
                        "sdm" = part_formulas[c("O_","D_","I_")],
                        "inst" = part_formulas[c("O_","D_","I_","G_")])
  #... data
  data_sources <- list("orig" = net_dat_letters %>% cols_keep("XX"),
                       "pair" = pair_dat_letters %>% cols_keep(c("YY","GG")))

  #... tests
  actual <- by_source_model_matrix(part_formulas, data_sources)
  expect_orig <- model.matrix( ~ I(XX^2) -1 ,data = net_dat_letters)
  expect_pair <- model.matrix( ~ exp(YY) + exp(GG) -1 ,data = pair_dat_letters)
  expect_equal(actual$orig,expect_orig, check.attributes = FALSE)
  expect_equal(actual$pair,expect_pair, check.attributes = FALSE )
})

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

