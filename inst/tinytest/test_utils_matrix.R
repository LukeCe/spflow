# ---- rbind_fill_left---------------------------------------------------------
expect_equal({
  spflow:::rbind_fill_left(matrix(1:6,nrow = 2),
                           matrix(1:4,nrow = 2),
                           matrix(1:2,nrow = 2))
  },
  {
    rbind(matrix(1:6,nrow = 2),
          cbind(c(NA,NA),matrix(1:4,nrow = 2)),
          cbind(c(NA,NA),c(NA,NA),matrix(1:2,nrow = 2)))
    })

expect_equal({
  spflow:::rbind_fill_left(matrix(1:6,nrow = 2),
                           matrix(1:4,nrow = 2),
                           matrix(1:2,nrow = 2),fill = 0)
},
{
  rbind(matrix(1:6,nrow = 2),
        cbind(c(0,0),matrix(1:4,nrow = 2)),
        cbind(c(0,0),c(0,0),matrix(1:2,nrow = 2)))
})


# ---- stack_columns ----------------------------------------------------------
expect_equal({
  test_mat <- matrix(letters[1:4],2,2,
                     dimnames = list(LETTERS[1:2],LETTERS[3:4]))
  spflow:::stack_columns(test_mat)
  },
  {
    data.frame(col = factor(c("C","D","C","D")),
               row = factor(c("A","A","B","B")),
               value = letters[1:4])
  })

# ---- trace_sequence ---------------------------------------------------------
expect_equal(spflow:::trace_sequence(diag(3),max_power = 5), rep(3,5))

