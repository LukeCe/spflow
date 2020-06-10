# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# These function are used to create vectorised references of the model model
# matrices.
# The more performant matrix based implementations are tested against these
# references.
# - - - - - - - - - - - - - - - - - - -
# Date: mai 2020

expand_O_D_I <- function(X) {
  named_list(c("DX","OX","IX"),as.matrix(X))
}
expand_O_D_I_mem <- memoise::memoise(expand_O_D_I)

vec_reference_O_D_I <- function(X) {

  result <- named_list(c("DX","OX","IX"))

  n_o <- nrow(X$OX)
  n_d <- nrow(X$DX)
  n_I <- nrow(X$IX)
  result$DX <- X$DX %x% rep(1, n_o)
  result$OX <- rep(1, n_d) %x% X$OX
  result$IX <- X$IX %|!|% (as.vector(diag(n_I)) * X$DX %x% rep(1, n_o))

  result <- result %>%
    compact() %>%
    reduce(cbind) %>%
    as.matrix()

  return(result)
}
vec_reference_O_D_I_mem <- memoise::memoise(vec_reference_O_D_I)

vec_reference_matrix <- function(...) {
  list(...) %>%
    flatlist() %>%
    lapply(as.vector) %>%
    reduce(cbind) %>%
    return()
}
vec_reference_matrix_mem <- memoise::memoise(vec_reference_matrix)

crossprod_mem <- memoise::memoise(crossprod)
