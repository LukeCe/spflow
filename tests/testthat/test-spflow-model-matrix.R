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

