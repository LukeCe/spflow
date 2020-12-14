y_b <- rnorm(40) %>% matrix(ncol = 4)

TSS <- crossprod(y_b)
tau <- c(1 , -0.5, - 0.4, 0.2)
tau_c <- c(1 , -0.6, - 0.4, 0.2)


# traditional computation
RSS <- tau %*% TSS %*% tau
RSS_c <- tau_c %*% TSS %*% tau_c
diff_RSS <- RSS_c - RSS

# new computation
tau_diff_plus <- tau_c + tau
tau_diff_minus <- tau_c - tau
diff2_RSS <- tau_diff_plus %*% TSS %*% tau_diff_minus
diff2_RSS <- tau_diff_minus %*% TSS %*% tau_diff_plus
