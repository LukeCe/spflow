moments <- modules::module({

  empirical_var <- function(flow_model_matrix) {

    # The moment is grouped into (4x4) block
    # ([alpha] [alpha_I] [beta] [gamma])^2
    # Of those 16 blocks 10 are unique and 6 are inferred by symmetry

    # The first 4 blocks correspond to the interactions with alpha
    alpha_blocks <-
      "alpha" %p% c("","_alpha_I","_beta","_gamma") %>%
      named_list()

    alpha_blocks[["alpha"]] <- moments$var$alpha(N = flow_model_matrix$N)


  }
  var <- modules::module({
    # ---- Variance Moments ---------------------------------------------------
    # ---- Diagonal Blocks ----
    modules::export("alpha","alpha_I","beta","gamma")

    alpha <- function(N) {N}

    alpha_I <- function(const_intra) {

      if (is.null(const_intra))
        return(NULL)

      # preallocate empty block matrix
      block_size <- length(const_intra)
      block_alpha_I <- matrix(nrow = block_size, ncol = block_size)

      block_alpha_I[1,1] <- nrow(const_intra$In)

      if (block_size == 1)
        return(block_alpha_I)

      # First row and columns are equal to the trace
      rows <- 1
      index <- cols <- 2:block_size
      block_alpha_I[rows,cols] <- block_alpha_I[cols, rows] <-
        rapply(const_intra[index], function(.w) sum(diag(.w)))

      # Remaining rows and columns correspond to the "hadaramad sum"
      rows <- cols <- 2:block_size
      block_alpha_I[rows,cols] <- hadamarad_sum_matrix(const_intra[index])

      return(block_alpha_I)
    }

    beta <- function(X) {

      # The beta block is split into [D,O,I]
      order_keys <- c("DX","OX","IX")
      X <- X[order_keys] %>% compact()

      # The diagonal sub blocks define the dimensions
      # They correspond to the scaled inner product of each matrix
      scaling <- derive_scalings(X)
      if (is.null(scaling))
        return(NULL)

      block_beta <-
        map2(X,scaling, function(.X , .scale) .scale * crossprod(.X)) %>%
        Matrix::bdiag()

      # Of diagonal blocks are filled by index
      indexes <-
        lapply(X, function(.X)  seq_len(ncol(.X))) %>%
        sequentialize_index()

      rows <- indexes$DX
      cols <- indexes$OX
      block_beta[rows,cols] <- tcrossprod(colSums(X$DX),colSums(X$OX))

      if (is.null(X$IX)) {
        return(forceSymmetric(block_beta,"U"))
      }

      # The last off-diagonal blocks correspond to the inner products:
      # (OX'IX) and (DX'IX)
      od_keys <- order_keys[1:2]
      rows <- indexes[od_keys] %>% flatten()
      cols <- indexes$IX
      block_beta[rows,cols] <-
        lapply(X[od_keys], crossprod, X$IX) %>%
        reduce(rbind)


      return(forceSymmetric(block_beta,"U"))
    }

    gamma <- function(G) {
      G %|!|% hadamarad_sum_matrix(G)
    }

    # ---- Off-diagonal Blocks ----
    modules::export("alpha_alpha_I","alpha_beta","alpha_gamma",
                    "alpha_I_beta", "alpha_I_gamma",
                    "beta_gamma")

    alpha_alpha_I <- function(const_intra) {
      const_intra %|!|% rapply(const_intra,sum)
    }

    alpha_beta <- function(X) {
      scaled_col_sums <-
        map2(
          .x = X %>% lapply(colSums),
          .y = X %>% derive_scalings(),
          .f = `*`) %>%
        reduce(c)

      return(scaled_col_sums)
    }

    alpha_gamma <- function(G) {
      G %|!|% rapply(G,sum)
    }

    alpha_I_beta <- function(const_intra,X) {

      if (is.null(X) || is.null(const_intra))
        return(NULL)

      result <-
        lapply(const_intra, matrix_prod_O_D_I, X) %>%
        reduce(rbind)

      return(result)
    }

    alpha_I_gamma <- function(const_intra,G) {

      if (is.null(G) || is.null(const_intra))
        return(NULL)

      size_gamma <- length(G)
      size_alpha_I <- length(const_intra)
      block_alpha_I_gamma <- matrix(0, nrow = size_alpha_I, ncol = size_gamma)

      block_alpha_I_gamma[1,] <- rapply(G, function(x) sum(diag(x)))

      if (size_alpha_I == 1)
        return(block_alpha_I_gamma)

      # sum of all element for each combination of two matrices
      had_sum_G <- function(.w) lapply(G, hadamarad_sum, .w) %>% unlist()

      block_alpha_I_gamma[-1,] <-
        const_intra[-1] %>%
        lapply(had_sum_G) %>%
        reduce(rbind)

      return(block_alpha_I_gamma)
    }

    beta_gamma <- function(X,G) {

      if (is.null(X) || is.null(G))
        return(NULL)

      result <-
        lapply(G, matrix_prod_O_D_I, X) %>%
        lapply(t) %>%
        reduce(cbind)

      return(result)
    }

  })


  empirical_covar <- function() {

  }
  cov <- modules::module({
    # ---- Covariance Moments -------------------------------------------------
    modules::export("alpha","alpha_I","beta","gamma")

    alpha <- function(Y) {
      sum(Y)
    }

    alpha_I <- function(Y,const_intra) {

      if (is.null(const_intra))
        return(NULL)

      block_Y_alpha_I <- sum(diag(Y))

      if (length(const_intra) == 1)
        return(block_Y_alpha_I)

      block_Y_alpha_I <- c(
        block_Y_alpha_I,
        unlist(lapply(const_intra[-1], "hadamarad_sum", Y))
      )

      return(block_Y_alpha_I)

    }

    beta <- function(Y, X) {
      matrix_prod_O_D_I(mat = Y,X = X)
    }

    gamma <- function(Y, G) {
      G %|!|% unlist(lapply(G,hadamarad_sum,Y))
    }


  })

   # ---- Helpers ------------------------------------------------------------
  define_moment_blocks <- function() {
    c("alpha","alpha_I","beta","gamma")
  }

  derive_scalings <- function(X) {

     if (is.null(X))
       return(NULL)

     scaling <- rapply(X[c("OX","DX","IX")], nrow)
     if (is.null(X$IX))
       return(scaling)

     scaling[3] <- 1
     return(scaling)
   }

   matrix_prod_O_D_I <- function(mat,X) {

     if (is.null(X) || is.null(mat))
       return(NULL)

     result <- with(X,expr = {
       list(
         DX %|!|% colSums(mat) %*% DX,
         OX %|!|% rowSums(mat) %*% OX,
         IX %|!|% diag(mat) %*% IX
       )}) %>%
       reduce(cbind)

     return(result)
   }


})
