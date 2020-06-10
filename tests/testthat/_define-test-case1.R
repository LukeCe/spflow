# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# The script creates a set of objects which is used as inputs for the matrix
# based calculation of the moments.
# It also provides a vecotr reference which is used as a benchmark for the
# correctness of the more efficient, but also more complex matrix formulation.
# - - - - - - - - - - - - - - - - - - -
# Date: june 2020

# for each object create an example input value
# and a vector reference to test against
case_description <-
  "Test data for the symmetric OD flows using simulated data of " %p%
  "16 german states. The simulated flows use the intra coefficients" %p%
  "and also SDM variables."


# ---- input values -----------------------------------------------------------
load_all()
data("germany_grid")
test_input <-
  named_list(c("N","W","const","const_intra","X","G","Y"))
test_vector_reference <-
  named_list(c("const","const_intra","X","G","Y"))

# number of observations
test_input$N <- nrow(germany_grid)^2

# the spatial weight matrix
test_input$W <-
  germany_grid %>%
  spdep::poly2nb(.) %>%
  spdep::nb2listw(.) %>%
  spdep::listw2mat(.) %>%
  Matrix::Matrix(.)

# the global constant
test_input$const <- 1
test_vector_reference$const <- rep(1,test_input$N)

# the intra regional constant
temp_intra <- spflow:::named_list(c("V","WV","WW"))
temp_intra$V <- Matrix::tcrossprod(test_input$W) # def. V = WW'
temp_intra$WV <- test_input$W %*% temp_intra$V
temp_intra$WW <- test_input$W %*% test_input$W

test_input$const_intra <- list(
  "In" = Matrix::Diagonal(nrow(test_input$W)),
  "W" = test_input$W,
  "W'" = t(test_input$W),
  "WW" = temp_intra$WW,
  "WW'" = t(temp_intra$WW),
  "V" = temp_intra$V,
  "VV" = Matrix::tcrossprod(temp_intra$WV, test_input$W),
  "WV" = temp_intra$WV,
  "VW'" = t(temp_intra$WV)
)
test_vector_reference$const_intra <-
  vec_reference_matrix(test_input$const_intra)
rm(temp_intra)

# origin, destinatuion and intra-regional attributes
test_input$X <-
  cbind("X" = germany_grid$X,
        "X_lag" = test_input$W %*% germany_grid$X) %>%
  expand_O_D_I()

test_vector_reference$X <-
  vec_reference_O_D_I(test_input$X)

# the distance matrix
test_input$G <- as.matrix(log(dist(sp::coordinates(germany_grid)) + 1))
test_vector_reference$G <- as.vector(test_input$G)

# ---- simulated flows --------------------------------------------------------
data("simulation_parameters")
test_input_sim <- named_list(c("Wd","Wo","Ww","A_inv","Z",
                               "error","signal","noise"))

In <- Matrix::Diagonal(nrow(test_input$W))
test_input_sim$Wd <- In %x% test_input$W
test_input_sim$Wo <- test_input$W %x% In
test_input_sim$Ww <- test_input$W %x% test_input$W

rho_model9 <- c("rho_d", "rho_o", "rho_w")
A <- mapply("*", rho[rho_model9], test_input_sim[c("Wd","Wo","Ww")],
            SIMPLIFY = FALSE) %>% Reduce("+",.)
A <- (In %x% In) - A
test_input_sim$A_inv <- solve(A)
rm(A,In)

test_input_sim$Z <-
  cbind(
    test_vector_reference$const,
    test_vector_reference$const_intra[, 1],
    test_vector_reference$X,
    test_vector_reference$G
  ) %>%
  as.matrix()

test_input_sim$error <- rnorm(test_input$N,sd = sd_error)
test_input_sim$noise <- test_input_sim$A_inv %*% test_input_sim$error
test_input_sim$signal <- test_input_sim$A_inv %*% (test_input_sim$Z %*% delta)
test_vector_reference$Y <- test_input_sim$noise + test_input_sim$noise
test_input$Y <- matrix(test_vector_reference$Y,nrow = nrow(test_input$W))
test_input$Y <- list(test_input$Y,
                     test_input$W %*% test_input$Y,
                     tcrossprod(test_input$Y, test_input$W),
                     test_input$W %*% tcrossprod(test_input$Y, test_input$W))

test_vector_reference$Y <-  vec_reference_matrix(test_input$Y)

# ---- reference moments ------------------------------------------------------
reference_moments <- named_list(c("N","HH","HY","ZZ","ZY","TSS"))

instrumental_variables <- list(
  "const" = FALSE,
  "intra_const" = c(FALSE,rep(TRUE,8)),
  "X" = rep(c(FALSE,FALSE,TRUE,TRUE),3),
  "G" = c(FALSE,TRUE,TRUE)
)

# X matrices with instruments
test_X_inst <- cbind("X" = germany_grid$X,
                     "X_lag" = test_input$W %*% germany_grid$X)

test_X_inst <- cbind(test_X_inst,
                     test_input$W %*% test_input$W %*% test_X_inst)
test_input$X <- test_X_inst %>% expand_O_D_I()
test_vector_reference$X <- test_input$X %>% vec_reference_O_D_I()

# G matrix with instruments
test_G_lag <- tcrossprod(test_input$W %*% test_input$G,test_input$W)
test_G_lag2 <- tcrossprod(test_input$W %*% test_G_lag,test_input$W)
test_input$G <- list(test_input$G,test_G_lag,test_G_lag2)
test_vector_reference$G <- vec_reference_matrix(test_input$G)
rm(test_X_inst,test_G_lag,test_G_lag2)

# reference H
test_H <- cbind(test_vector_reference$const,
                test_vector_reference$const_intra,
                test_vector_reference$X,
                test_vector_reference$G)


reference_moments$N <- test_input$N
reference_moments$HH <- crossprod(test_H)
reference_moments$HY <- crossprod(test_H,test_vector_reference$Y)
reference_moments$ZZ <- crossprod(test_input_sim$Z)
reference_moments$ZY <- crossprod(test_input_sim$Z, test_vector_reference$Y)
reference_moments$TSS <- crossprod(test_vector_reference$Y)

save(case_description,
     instrumental_variables,
     reference_moments,
     test_input,
     test_input_sim,
     test_vector_reference,
     file = "tests/testthat/test_case_1.rda")

