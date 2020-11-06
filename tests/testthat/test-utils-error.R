test_that("assert: => correct errors", {

  # default assertion
  expect_error(assert(FALSE))
  expect_warning(assert(FALSE,warn = TRUE))
  expect_invisible(assert(TRUE))

  # class assertion
  false_input <- NULL
  expect_error(
    assert_is(false_input,"data.frame"),
    regexp = "The input argument false_input must be of class data.frame!")
  expect_invisible(assert_is(cars,"data.frame"))
})


