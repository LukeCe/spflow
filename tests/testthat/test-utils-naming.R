test_that("drop_lnames: => correct output", {

  actual_null <- drop_lnames(NULL)
  expect_null(actual_null)

  actual <- list("A" = list("A" = 1:10), "B"= 11:20) %>% drop_lnames()
  expected <- list(list("A" = 1:10), 11:20)
  expect_equal(actual, expected)
})

test_that("lookup: => correct output", {

  actual_null <- lookup(NULL)
  expect_null(actual_null)

  actual <- lookup(letters[1:5])
  expected_values <- expected_names <- letters[1:5]
  expect_equal(actual %>% names()    , expected_names)
  expect_equal(actual %>% as.vector(), expected_values)
})

test_that("list_lookup: => correct output", {

  names <- c("A","B","C")
  values <- 1:3
  actual <- list_lookup(values,names)
  expected <- list("A" = 1, "B" = 2, "C" = 3)
  expect_equal(actual, expected)
})


