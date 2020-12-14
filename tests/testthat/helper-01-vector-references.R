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
  named_list(c("D_","O_","I_"),as.matrix(X))
}

vec_reference_O_D_I <- function(X) {

  result <- named_list(c("D_","O_","I_"))

  n_o <- nrow(X$O_)
  n_d <- nrow(X$D_)
  n_I <- nrow(X$I_)
  result$D_ <- rep(1, n_d) %x% X$O_
  result$O_ <- X$D_ %x% rep(1, n_o)
  result$I_ <- X$I_ %|!|% (as.vector(diag(n_I)) * X$D_ %x% rep(1, n_o))

  result <- result %>%
    compact() %>%
    lreduce(cbind) %>%
    as.matrix()

  return(result)
}
vec_reference_matrix <- function(...) {
  list(...) %>%
    flatlist() %>%
    lapply(as.vector) %>%
    lreduce(cbind) %>%
    return()
}
