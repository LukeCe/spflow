test_that("pull_flow_data: => correct output", {

  example_net <- multi_net_usa_ge
  dat_usa  <- dat(network_nodes(example_net,"usa"))
  dat_ge   <- dat(network_nodes(example_net,"ge"))
  dat_pair_usa_ge <- dat(network_pairs(example_net,"usa_ge"))
  dat_pair_ge_ge <- dat(network_pairs(example_net,"ge_ge"))

  # test case o != d
  pair_id <- "usa_ge"
  actual <- pull_flow_data(example_net,pair_id)
  expected <- list("orig" = dat_usa, "dest" = dat_ge, "pair" = dat_pair_usa_ge)
  expect_equal(actual, expected)

  # test case o != d
  pair_id <- "ge_ge"
  actual <- pull_flow_data(example_net,pair_id)
  expected <- list("orig" = dat_ge, "pair" = dat_pair_ge_ge)
  expect_equal(actual, expected)
})

# ---- model matrix creation --------------------------------------------------
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

test_that("combine_formulas_by_source: => correct output", {

  formula_parts <- list("Y_" = ~ y, "G_" =  ~ dist,
                        "D_" = ~ a + b, "O_" = ~ b + c, "I_" = ~ d) %>%
    lapply("remove_constant")

  # test case 1 (orig == dest)
  actual <- combine_formulas_by_source(c("pair","orig"),formula_parts)
  expected <- list("pair"= ~ y + dist - 1,
                   "orig" = ~ a + b + c + d - 1)
  expect_equal(actual, expected)

  # test case 1 (orig != dest)
  actual <- combine_formulas_by_source(c("pair","orig","dest"),formula_parts)
  expected <- list("pair" = ~ y + dist - 1,
                   "dest" = ~ a + b - 1,
                   "orig" = ~ b + c - 1)
  expect_equal(actual, expected)
})

test_that("flow_conform_model_matrix: => correct output", {

  ### Zero factors
  # test 1: with intercept
  expected <- model.matrix(~ . , cars) %>% cols_drop("(Intercept)")
  actual <- flow_conform_model_matrix(~ . , cars)
  expect_equal(actual, expected)

  # test 2: no intercept
  expected <- model.matrix(~ . , cars) %>% cols_drop("(Intercept)")
  actual <- flow_conform_model_matrix(~ . - 1 , cars)
  expect_equal(actual, expected)

  ### One factor
  # test 1: with intercept
  expected <- model.matrix(~ . -1, iris)
  actual <- flow_conform_model_matrix(~ . , iris)
  expect_equal(actual, expected)

  # test 2: no intercept
  expected <- model.matrix(~ . -1, iris)
  actual <- flow_conform_model_matrix(~ . - 1, iris)
  expect_equal(actual, expected)

  ### Two factors
  dat_two_fact <- as.data.frame(ChickWeight)

  # test 1: with intercept
  expected <- model.matrix(~ . -1, dat_two_fact)
  actual <- flow_conform_model_matrix(~ ., dat_two_fact)
  expect_equal(actual, expected)

  # test 2: no intercept
  expected <- model.matrix(~ . -1, dat_two_fact)
  actual <- flow_conform_model_matrix(~ . - 1, iris)
  expect_equal(actual, expected)

})

test_that("flow_conform_model_matrix: with centering => correct output", {

  ### Zero factors
  # test 1: with intercept
  expected <- model.matrix(~ center(speed) + center(dist) -1 , cars)
  actual <- flow_conform_model_matrix(~ center(speed) + center(dist) , cars)
  expect_equal(actual, expected, check.attributes = FALSE)

  expected <- model.matrix(~ center(log(speed)) + center(log(dist)) -1 , cars)
  actual <- flow_conform_model_matrix(~ center(log(speed)) + center(log(dist)),
                                      cars)
  expect_equal(actual, expected, check.attributes = FALSE)

})

# ---- lagged variable creation -----------------------------------------------



test_that("by_role_spatial_lags: => correct output", {

  ## Define the test case based on the alphabet test data...
  #... formulas
  test_formula <- YY ~ XX + G_(GG)
  part_variables <-
    c("Y_" = "exp(YY)", "G_" = "exp(GG)", lookup("I(XX^2)", c("O_","D_","I_")))
  var_to_form <- function(v) {reformulate_string(v) %>% remove_constant()}
  part_formulas <- part_variables %>% lapply("var_to_form")
  part_formulas <- list("norm" = part_formulas,
                        "sdm" = part_formulas[c("O_","D_","I_")],
                        "inst" = part_formulas[c("O_","D_","I_","G_")])
  #... model matrices
  model_matrices <- list(
    "orig" = model.matrix( ~ I(XX^2) -1 ,data = net_dat_letters),
    "pair" = model.matrix( ~ exp(YY) + exp(GG) -1 ,data = pair_dat_letters))

  #...tests
  actual <- by_role_spatial_lags(model_matrices,)

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








  actual_null <- by_role_spatial_lags(source_model_matrices = ,
                                      lag_requirements = ,
                                      neighborhoods = )
  expect_null(actual_null)

  actual <- by:rol()
  expected <- reference
  expect_equal(actual, expected)
})





# ============== OLD =================
# spflow_model_matrix ---------------------------------------------------------
test_that("spflow_model_frame: => correct output", {

  # standard case
  example_net <- sp_network_nodes("cars",node_data = cars)
  example_formula <- list("O" = ~speed + dist,
                          "D" = ~log(speed) + log(dist))

  actual <- flow_model_frame(example_net,example_formula)
  expected <- model.matrix.default(~speed + dist + log(speed) + log(dist) - 1,
                                   data = cars)
  expect_equal(actual, expected)


  # unknown variables
  example_formula$O <- ~sppeedd + disttt
  expect_error(
    flow_model_frame(example_net,example_formula),
    "^The variables \\[sppeedd and disttt\\] were not found .*" %p%
      "with id \\[cars\\]\\!$")

})


test_that("split_by_source: => correct output", {

  cases <- c("inst","norm","sdm")
  example_net <- sp_network_nodes("cars",node_data = cars)
  example_formula <- list("OX" = ~speed + dist,
                          "DX" = ~log(speed) + log(dist),
                          "IX" = ~log(speed + 1) + log(dist + 1)) %>%
    lapply(function(.f) named_list(cases, .f))
  example_design_matrix <-
    model.matrix(data = cars,
                 ~speed + dist + log(speed) + log(dist) +
                   log(speed + 1) + log(dist + 1) - 1 )

  lag_suffixes <- c("",".lag1",".lag2",".lag3")
  example_design_matrix_lagged <-
    cbind(suffix_columns(example_design_matrix * 1, lag_suffixes[1]),
          suffix_columns(example_design_matrix * 2, lag_suffixes[2]),
          suffix_columns(example_design_matrix * 3, lag_suffixes[3]),
          suffix_columns(example_design_matrix * 4, lag_suffixes[4]))


  actual <- split_by_source(
    global_design_matrix = example_design_matrix_lagged,
    node_formulas = example_formula,
    node_data_template = data.table::as.data.table(cars))


  ## Test naming and instrument status of all three cases
  expected_inst_status <- rep(c(FALSE,TRUE),each = 4)
  lag_suffixes <- c("",".lag1",".lag2",".lag3")

  # origin case
  expected_names <- lapply(lag_suffixes, function(.suf) {
    "Orig_" %p% c("speed","dist") %p% .suf
  }) %>% flatten()

  expect_equal(colnames(actual$OX), expected_names)
  expect_equal(get_instrument_status(actual$OX), expected_inst_status)

  # destination case
  expected_inst_status <- rep(c(FALSE,TRUE),each = 4)
  expected_names <- lapply(lag_suffixes, function(.suf) {
    "Dest_" %p% c("log(speed)", "log(dist)") %p% .suf
  }) %>% flatten()

  expect_equal(colnames(actual$DX), expected_names)
  expect_equal(get_instrument_status(actual$DX), expected_inst_status)

  # intra case
  expected_inst_status <- rep(c(FALSE,TRUE),each = 4)
  expected_names <- lapply(lag_suffixes, function(.suf) {
    "Intra_" %p% c("log(speed + 1)", "log(dist + 1)") %p% .suf
  }) %>% flatten()

  expect_equal(colnames(actual$IX), expected_names)
  expect_equal(get_instrument_status(actual$IX), expected_inst_status)
})

