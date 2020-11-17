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

  # multi-class assertion
  false_input <- NULL
  expect_error(
    assert_is_one_of(false_input,c("A","B")),
    regexp = "The input argument false_input must be of class A or B!")
  expect_invisible(assert_is_one_of(cars,c("data.frame","matrix")))

  # length 1 input assertion
  error_template <-
    "The input argument false_input must be a %s of length one!"

  class <- "character"
  false_input <- NULL
  true_input <- "true"
  expect_error(assert_is_single_x(false_input,class),
               sprintf(error_template, class))
  expect_invisible(assert_is_single_x(true_input,x = class))

  class <- "logical"
  false_input <- NULL
  true_input <- TRUE
  expect_error(assert_is_single_x(false_input,class),
               sprintf(error_template, class))
  expect_invisible(assert_is_single_x(true_input,class))

})
