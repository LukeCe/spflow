test_that("drop_names: => correct output", {

  actual_null <- drop_names(NULL)
  expect_null(actual_null)

  actual <- list("A" = list("A" = 1:10), "B"= 11:20) %>% drop_names()
  expected <- list(list("A" = 1:10), 11:20)
  expect_equal(actual, expected)
})

