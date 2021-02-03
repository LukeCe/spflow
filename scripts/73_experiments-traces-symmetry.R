devtools::load_all()
A <- matrix(rnorm(16),4,4)
B <- 0.5 * (t(A) + A)

sum(diag(A))
sum(diag(A %*% A))
trace_sequence(A)
trace_sequence(B)



x <- rnorm(4)
x %*% A %*% x
x %*% B %*% x

x %*% A %*% A %*% x
x %*% B %*% B %*% x
