# ---- interpret_flow_formula -------------------------------------------------

expect_equal({
  spflow:::interpret_flow_formula(
    y ~ D_(a) + O_(b) + I_(c) + G_(d),
    list(use_intra = FALSE, sdm_variables = "none", estimation_method = "mle")
    )
  },
  {
    list(
      constants = list("global" = TRUE),
      norm      = list("Y_" = ~ y - 1, "D_" = ~ a - 1,
                       "O_" = ~ b - 1, "G_" = ~ d - 1)
    )
  },
  info = "formulas is split correctly (no sdm) (no intra)")

expect_equal({
  spflow:::interpret_flow_formula(
    y ~ D_(a) + O_(b) + I_(c) + G_(d),
    list(use_intra = FALSE, estimation_method = "mle",
         sdm_variables = "same")
  )
  },
  {
    list(
      constants = list("global" = TRUE),
      norm      = list("Y_" = ~ y - 1, "D_" = ~ a - 1,
                       "O_" = ~ b - 1, "G_" = ~ d - 1),
      sdm       = list("D_" = ~ a - 1, "O_" = ~ b - 1)
      )
  },
  info = "formulas is split correctly (with sdm) (no intra)")

expect_equal({
    spflow:::interpret_flow_formula(
      y ~ D_(a) + O_(b) + I_(c) + G_(d),
      list(use_intra = TRUE, estimation_method = "mle",
           sdm_variables = "same")
    )
  },
  {
    list(
      constants = list("global" = TRUE, "intra" = TRUE),
      norm      = list("Y_" = ~ y - 1, "D_" = ~ a - 1, "O_" = ~ b - 1,
                       "I_" = ~ c - 1, "G_" = ~ d - 1),
      sdm       = list("D_" = ~ a - 1, "O_" = ~ b - 1, "I_" = ~ c - 1)
    )
  },
  info = "formulas is split correctly (with sdm) (with intra)")

expect_equal({
    spflow:::interpret_flow_formula(
      y ~ D_(a) + O_(b) + I_(c) + G_(d),
      list(use_intra = TRUE,
           estimation_method = "s2sls",
           sdm_variables = "same",
           twosls_instrumental_variables = "same")
    )
  },
  {
    list(
      constants = list("global" = TRUE, "intra" = TRUE),
      norm      = list("Y_" = ~ y - 1, "D_" = ~ a - 1, "O_" = ~ b - 1,
                       "I_" = ~ c - 1, "G_" = ~ d - 1),
      sdm       = list("D_" = ~ a - 1, "O_" = ~ b - 1, "I_" = ~ c - 1),
      inst      = list("D_" = ~ a - 1, "O_" = ~ b - 1,
                       "I_" = ~ c - 1, "G_" = ~ d - 1)
    )
  },
  info = "formulas is split correctly (with sdm, intra, instruments)")



expect_equal({
    spflow:::interpret_flow_formula(
      y ~ . + log(a + 1)+ log(b + 1) + I_(c) + G_(d) - 1,
      list(use_intra = TRUE,
           estimation_method = "s2sls",
           sdm_variables = "same",
           twosls_instrumental_variables = "same")
    )
  },
  {
    list(
      constants = list("global" = FALSE, "intra" = TRUE),
      norm      = list("Y_" = ~ y - 1,
                       "D_" = ~ . + log(a + 1) + log(b + 1) - 1,
                       "O_" = ~ . + log(a + 1) + log(b + 1) - 1,
                       "I_" = ~ c - 1, "G_" = ~ d - 1),
      sdm       = list("D_" = ~ . + log(a + 1) + log(b + 1) - 1,
                       "O_" = ~ . + log(a + 1) + log(b + 1) - 1,
                       "I_" = ~ c - 1),
      inst      = list("D_" = ~ . + log(a + 1) + log(b + 1) - 1,
                       "O_" = ~ . + log(a + 1) + log(b + 1) - 1,
                       "I_" = ~ c - 1, "G_" = ~ d - 1)
    )
  },
  info = "use of dot shortcut for unspecifyed specials")


