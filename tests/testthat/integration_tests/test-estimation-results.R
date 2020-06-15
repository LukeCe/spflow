# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - integration test for data transformation steps
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# Test if the top level formula is interpreted corrextly and all the data
# required for model estimation is constructed.
# - - - - - - - - - - - - - - - - - - -
# Date: May 2020

load_all()
data("multi_net_examples")

# ---- setup test examples ----------------------------------------------------
model2_formula_shortcut <- y2 ~ .
model2_formula_complete <- y2 ~ O_(X) + D_(X) + I_(X) + G_(distance)

model9_formula_shortcut <- y9 ~ .
model9_formula_complete <- y9 ~ O_(X) + D_(X) + I_(X) + G_(distance)

# ---- formula and control-----------------------------------------------------
test_that("Shortcut notations have same results", {
  result2 <- spflow(model9_formula_complete,multi_net_examples,"ge_ge")
  result2_short <- spflow(model2_formula_shortcut,multi_net_examples,"ge_ge")

  expect_equal(object = spflow())
})
