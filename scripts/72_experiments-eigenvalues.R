library("dplyr")
library("data.table")

set.seed(123)
s <- 6
i_s <- rep(1,s)
a <- matrix(abs(rnorm(s^2)),s)
b <- matrix(abs(rnorm(s^2)),s)

diag(a)  <- 0
diag(b)  <- 0
a <- a / rowSums(a)
b <- b / rowSums(b)

cc1 <- b
cc2 <- b
A1 <- kronecker(diag(s),cc1)
A2 <- kronecker(cc2,diag(s))
A3 <- kronecker(cc2,cc1)


# EV 1 - 2
eigen(A1 + A2)$values %>% sort()
(eigen(A1)$values + eigen(A2)$values[c(1,4,7,2,5,8,3,6,9)] )%>% sort()


# EV 1 - 3
data.frame(EV = eigen(cc1)$values %x% eigen(cc1)$values,1:9) %>%
  arrange(EV) %>%
  pull(X1.9) -> order_W

fac1 <-  0.8
fac3 <-  0.8
EV_13_a <- eigen(fac1 * A1 + fac3 * A3)$values %>% sort()
part_1 <- eigen(A1)$values
part_2 <- eigen(A3)$values

opt <- CJ(part_1,part_2,EV_13_a)
opt[, EV_opt := part_1 * fac1 + part_2 * fac3]
opt[, EV_diff := abs(EV_opt -  EV_13_a) <= 0.00000001]

opt[EV_diff == TRUE,EV_13_a] %>%  unique() %>% sort()
EV_13_a


###
fac1 <- 0.4
fac2 <- 0.6
fac3 <- -.36

EV1 <- eigen(cc1)$values
EV2 <- eigen(cc2)$values

EV_123_a <- eigen(fac1 * A1 + fac2 * A2 + fac3 * A3)$values %>% sort()
EV_123_b <-
  kronecker(i_s,EV1) * fac1 +
  kronecker(EV2,i_s)* fac2 +
  kronecker(EV2,EV1) * fac3

EV_123_b %>% sort()
EV_123_a %>% sort()


# EV 1 - 2 - 3
eigen(A1 + A2 + 0.5 * A3)$values %>% sort() %>% unique()
(eigen(A1)$values[] + eigen(A2)$values[c(1,4,7,2,5,8,3,6,9)] + eigen(0.5 * A3)$values[rev(order_W)] )%>% sort() %>% unique()




svd(A3)

eigen(diag(9) - A3 - A2 - A1)
1 - eigen(A3)$values - eigen(A2)$values - eigen(A2)$values

