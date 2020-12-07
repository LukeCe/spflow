test_that("rbind_fill_left: => correct output", {

  mat_6 <- matrix(1:6,nrow = 2)
  mat_4 <- matrix(1:4,nrow = 2)
  mat_2 <- matrix(1:2,nrow = 2)

  # default
  actual <- rbind_fill_left(mat_6,mat_4,mat_2)
  expected <- rbind(mat_6,
                    cbind(c(NA,NA),mat_4),
                    cbind(c(NA,NA),c(NA,NA),mat_2))
  expect_equal(actual, expected)

  # zero
  actual <- rbind_fill_left(mat_6,mat_4,mat_2,fill = 0)
  expected[is.na(expected)] <- 0
  expect_equal(actual, expected)

})
