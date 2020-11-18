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

test_that("pull_neighborhood_data: => correct output", {

  actual <- pull_neighborhood_data(sp_multi_net_alphabet, "letters_LETTERS")
  expected_OW <- neighborhood(sp_net_letters)
  expected_DW <- neighborhood(sp_net_LETTERS)
  expect_equal(actual$OW, expected_OW)
  expect_equal(actual$DW, expected_DW)

})

test_that("def_matrix_form_args: => correct output", {

  # provides the right arguments to the vec to matrix function
  actual <- def_matrix_form_args(sp_multi_net_alphabet, "letters_LETTERS")
  test_vec <- pair_dat_letTERS$YY
  expect_silent({
    test_mat <- do.call(vec_to_matrix,args = c(list("vec" = test_vec), actual))
    })
  expect_equal(test_mat,matrix(test_vec,8,10))
})

test_that("define_flow_constants: => correct output", {

  # with instrument and intra
  constants <- list("global" = TRUE, "intra" = TRUE)
  actual <- define_flow_constants(constants,
                                  use_instruments = TRUE,
                                  OW = Diagonal(10)*3)
  expect_global <- 1
  expect_intra_names <- c("In","W","W'","WW","WW'","V","VV", "WV", "VW'")
  expect_equal(actual$global,expect_global)
  expect_equal(actual$intra %>% names(),expect_intra_names)

  # without instrument and intra
  constants <- list("global" = TRUE, "intra" = TRUE)
  actual <- define_flow_constants(constants,
                                  use_instruments = FALSE,
                                  OW = Diagonal(10)*3)
  expect_global <- 1
  expect_intra <- list("In" = Diagonal(10))
  expect_equal(actual$global,expect_global)
  expect_equal(actual$intra, expect_intra)
})

test_that("define_flow_weights: => correct output", {

  args <- def_matrix_form_args(sp_multi_net_alphabet, "letters_LETTERS")
  test_dat <- pair_dat_letTERS
  test_weight <- "GG"
  actual_null <- define_flow_weights(test_dat,NULL,args)
  expect_null(actual_null)

  actual <- define_flow_weights(test_dat,test_weight,args)
  expected <- test_dat[[test_weight]]
  expect_equal(as.vector(actual), expected)
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

