test_that("pull_flow_data: => correct output", {

  example_net <- sp_multi_net_alphabet
  dat_letters  <- dat(pull_nodes(example_net,"letters"))
  dat_LETTERS   <- dat(pull_nodes(example_net,"LETTERS"))
  dat_pair_letTERS <- dat(pull_pairs(example_net,"letters_LETTERS"))
  dat_pair_letters <- dat(pull_pairs(example_net,"letters_letters"))

  # test case o != d
  pair_id <- "letters_LETTERS"
  actual <- pull_flow_data(example_net,pair_id)
  expected <- list("orig" = dat_letters, "dest" = dat_LETTERS,
                   "pair" = dat_pair_letTERS) %>%
    lapply(function(.d) cols_drop(.d,key(.d)))
  expect_equal(actual, expected)

  # test case o == d
  pair_id <- "letters_letters"
  actual <- pull_flow_data(example_net,pair_id)
  expected <- list("orig" = dat_letters, "pair" = dat_pair_letters) %>%
    lapply(function(.d) cols_drop(.d,key(.d)))
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
  expect_intra <- list("In" = Diagonal(10) %>% set_instrument_status(FALSE))
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
