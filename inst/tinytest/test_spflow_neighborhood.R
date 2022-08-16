# ---- expand_spflow_neighborhood -----------------------------------------------
dg_Matrix <- function(x, d) as(diag(x, d, d),"Matrix")

expect_equivalent({
  W <- Matrix::Diagonal(2,2)
  spflow:::expand_spflow_neighborhood(W, W, model = "model_9")
},
{
  lapply(list("Wd" = 2, "Wo" = 2, "Ww" = 4), "dg_Matrix", 4)
},
info = "neighborhood expansion (model_9 + cartesian + square)")

expect_equivalent({
  OW <- Matrix::Diagonal(2,2)
  DW <- Matrix::Diagonal(3,3)
  spflow:::expand_spflow_neighborhood(OW, DW, model = "model_9")
},
{
  lapply(list("Wd" = 3, "Wo" = 2, "Ww" = 6),  "dg_Matrix", 6)
},
info = "neighborhood expansion (model_9 + cartesian + rectangular)")


expect_equivalent({
  Y_s <- matrix(c(1,1,1,0),2,2)
  W <- dg_Matrix(c(.5,2))
  spflow:::expand_spflow_neighborhood(W, W, model = "model_9",
                                    M_indicator = Y_s)
},
{
  dw <- c(.5,2)[c(1,2,1)]
  ow <- c(.5,2)[c(1,1,2)]
  lapply(list("Wd" = dw, "Wo" = ow, "Ww" = dw * ow), "dg_Matrix", 3)
},
info = "neighborhood expansion (model_9 + non-cartesian + square)")

expect_equivalent({
  OW <- dg_Matrix(2,2)
  DW <- dg_Matrix(3,3)
  Y_s <- matrix(c(1,0,1,0,0,1),3,2)
  spflow:::expand_spflow_neighborhood(OW, DW, model = "model_9",M_indicator = Y_s)
},
{
  lapply(list("Wd" = 3, "Wo" = 2, "Ww" = 6), "dg_Matrix", 3)
},
info = "neighborhood expansion (model_9 + non-cartesian + rectangular)")


# ---- normalize_neighborhood -------------------------------------------------
expect_equal({
  a <- matrix(1:25,5,5)
  diag(a) <- 0
  a / rowSums(a)
  }, {
  spflow:::normalize_neighborhood(matrix(1:25,5,5),by_row = TRUE)
  }
  , check.attributes = FALSE
  , info = "Row standardized neighborhood.")


expect_equal({
  a <- matrix(1:25,5,5)
  diag(a) <- 0
  a / abs(eigen(a)$values[1])
}, {
  spflow:::normalize_neighborhood(matrix(1:25,5,5),by_row = FALSE)
}
, check.attributes = FALSE
, info = "Row standardized neighborhood.")



