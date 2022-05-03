# ---- combine_formulas_by_source ---------------------------------------------
expect_equal({
  formula_parts <- list("Y_" = ~ y - 1, "G_" =  ~ dist - 1,
                        "D_" = ~ a + b - 1, "O_" = ~ b + c, "I_" = ~ d - 1)
  spflow:::combine_formulas_by_source(c("pair","orig"),formula_parts)
  },
  {
    list("pair"= ~ y + dist - 1,
         "orig" = ~ a + b + c + d - 1)
  },
  info = "combine all formulas that apply to the same data (no dest)")

expect_equal({
  formula_parts <- list("Y_" = ~ y - 1, "G_" =  ~ dist - 1,
                        "D_" = ~ a + b - 1, "O_" = ~ b + c - 1, "I_" = ~ d - 1)
  spflow:::combine_formulas_by_source(c("pair","orig","dest"),formula_parts)
  },
  {
    list("pair" = ~ y + dist - 1,
         "dest" = ~ a + b - 1,
         "orig" = ~ b + c - 1)
  },
  info = "combine all formulas that apply to the same data (with dest)")

# ---- flow_conform_model_matrix ----------------------------------------------
expect_equal(
  spflow:::flow_conform_model_matrix(~ . , data.frame("A" = 1:2, "B" = 3:4)),
  model.matrix(~ . , data.frame("A" = 1:2, "B" = 3:4))[,c("A","B")],
  info = "handles intercept removal",
  check.attributes = FALSE)

expect_equal(
  spflow:::flow_conform_model_matrix(~ . - 1 , data.frame("A" = 1:2, "B" = 3:4)),
  model.matrix(~ . - 1, data.frame("A" = 1:2, "B" = 3:4)),
  info = "handles intercept removal",
  check.attributes = FALSE)

expect_equal({
  spflow:::flow_conform_model_matrix(
    ~ . - 1,
    data.frame("A" = 1:2,
               "B" = 3:4,
               "C" = factor(c("D","F")))
    )
  },
  {
    model.matrix(
      ~ . - 1,
      data.frame("A" = 1:2,
                 "B" = 3:4,
                 "C" = factor(c("D","F")))
      )[,c("A","B","CF")]
  },
  info = "handles one factor (dont expand all levels)",
  check.attributes = FALSE)

expect_equal({
  spflow:::flow_conform_model_matrix(
    ~ . - 1,
    data.frame("A" = 1:4,
               "B" = 3:6,
               "C" = factor(c("D","F")),
               "H" = factor(c("G","I","K","K")))
  )
  },
  {
    model.matrix(
      ~ .,
      data.frame("A" = 1:4,
                 "B" = 3:6,
                 "C" = factor(c("D","F")),
                 "H" = factor(c("G","I","K","K")))
    )[,c("A","B","CF","HI","HK")]
  },
  info = "handles two factor (dont expand all levels)",
  check.attributes = FALSE)

