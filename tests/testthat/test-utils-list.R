test_that("flatlist: => correct output", {

  actual_null <- flatlist(NULL)
  expect_null(actual_null)

  nested_list <- list("A" = list("A1" = 1, "A2" = 2),
                      "B" = list("B1" = 1, "B2" = 2))
  actual <- flatlist(nested_list)
  expected <- list("A1" = 1, "A2" = 2, "B1" = 1, "B2" = 2)
  expect_equal(actual, expected, check.attributes = FALSE)

  actual <- flatlist(nested_list,use.names = FALSE)
  expected <- list("A1" = 1, "A2" = 2, "B1" = 1, "B2" = 2)

})
