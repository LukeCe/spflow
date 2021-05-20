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

# ---- validate_source_formulas -----------------------------------------------
expect_error({
  spflow:::validate_source_formulas(
    ~ A + B + not_avilable -1,
    data.frame("A" = 1, "B" = 1),
    "orig")
  },
  pattern = "^The variables \\[not_avilable\\] were not found in the data .*")

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

# ---- by_source_model_matrix -------------------------------------------------
expect_equal({
  data_sources <- list(
    pair = data.frame(y1 = 1, p1 = 1:4, p2 = 5:8),
    orig = data.frame(o1 = 1:4, o2 = 5:8))
  formula_roles <- list("Y_" = ~ log(y1),
                        "G_" = ~ p1,
                        "O_" = ~ o1,
                        "D_" = ~ log(o1))
  formula_roles2 <- list("Y_" = ~ log(y1),
                         "G_" = ~ p2,
                         "O_" = ~ o2,
                         "D_" = ~ log(o2))

  forumula_parts <- list("norm" = formula_roles,
                         "sdm"  = formula_roles2)
  lapply(spflow:::by_source_model_matrix(forumula_parts,data_sources),
         spflow:::sort_columns)

  },
  {
  lapply(
    list(pair = model.matrix(~ log(y1) + p1 + p2 - 1,
                             data.frame(y1 = 1, p1 = 1:4, p2 = 5:8)),
         orig = model.matrix(~ log(o1) + o1 + log(o2) +  o2 - 1,
                             data.frame(o1 = 1:4, o2 = 5:8))),
    spflow:::sort_columns)
  },
  info = "combine formulas and create a single design matrix by source")

expect_equal({
  data_sources <- list(
    pair = data.frame(y1 = 1, p1 = 1:4, p2 = 5:8),
    orig = data.frame(o1 = 1:4, o2 = 5:8),
    dest = data.frame(d1 = 1:4, d2 = 5:8))
  formula_roles <- list("Y_" = ~ log(y1),
                        "G_" = ~ p1,
                        "O_" = ~ o1,
                        "D_" = ~ log(d1))
  formula_roles2 <- list("Y_" = ~ log(y1),
                         "G_" = ~ p2,
                         "O_" = ~ o2,
                         "D_" = ~ log(d2))
  formula_roles3 <- list("G_" = ~ sqrt(p1),
                         "O_" = ~ sqrt(o1),
                         "D_" = ~ sqrt(d1))


  forumula_parts <- list("norm" = formula_roles,
                         "sdm"  = formula_roles2,
                         "inst" = formula_roles3)
  lapply(spflow:::by_source_model_matrix(forumula_parts,data_sources),
         spflow:::sort_columns)

  },
  {
    lapply(
      list(pair = model.matrix(~ log(y1) + p1 + p2 + sqrt(p1) - 1,
                             data.frame(y1 = 1, p1 = 1:4, p2 = 5:8)),
           orig = model.matrix(~ o1 + o2 + sqrt(o1) - 1,
                             data.frame(o1 = 1:4, o2 = 5:8)),
           dest = model.matrix(~ log(d1) +  log(d2) +  sqrt(d1) - 1,
                               data.frame(d1 = 1:4, d2 = 5:8))),
      spflow:::sort_columns)
  },
  info = "combine formulas and create a single design matrix by source")
