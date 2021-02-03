# look whether the determinant can serve as indicator for singularity
mat_a <- function(a){
  diag(c(1,a,1))
}

mat_b <- function(b){
  mat <- diag(3)
  bb <- cbind(b,c(1,1,1),b)
  mat - bb
}

seq_ab <- seq(-1,1,by = 0.01)
det_a <- sapply(seq_ab, function(.a) det(mat_a(.a)))
rcond_a <- sapply(seq_ab, function(.a) rcond(mat_a(.a)))
# cbind(seq_ab,det_a,rcond_a) %>% View()
det_b <- sapply(seq_ab, function(.b) det(mat_b(.b)))


# ...
library("spflow")
library("ggplot2")
library("data.table")
nb_ge <- neighborhood(germany_net)
nb_usa <- neighborhood(usa_net)
real_eigs <- function(mat){
  evs <- eigen(mat)$values
  re_evs <- Re(evs)[Re(evs) == evs] %>% lfilter(.f = function(x) x!= 0)
}

det_approx_2 <- function(traces,rho){
  n_approx <- length(traces)
  app_seq <- seq_along(traces)

  approx_value <- -sum(rho^(app_seq)*app_seq*traces)

}

det_Xapprox_2 <- function(traces,rho,x){
  n_approx <- length(traces)
  app_seq <- seq_along(traces)

  approx_value0 <- -sum(x^(app_seq)*traces/app_seq)
  approx_value1 <- -sum(rho^(app_seq-1)*traces*(rho-x))
  approx_value2 <- -sum(rho^(app_seq-2)*traces*(rho-x)^2*(app_seq-1))

  approx_value0 + approx_value1 + approx_value2
}

det_approx_4 <- function(traces,rho){
  n_approx <- length(traces)
  app_seq <- seq_along(traces)

  approx_value2 <- -sum(rho^(app_seq)*app_seq*traces)
  approx_value3 <- -sum(rho^(app_seq)*(app_seq-1)*(app_seq-2)*traces)
  approx_value4 <- -sum(rho^(app_seq)*(app_seq-1)*(app_seq-2)*(app_seq-3)*traces)

  approx_value2 + approx_value3 + approx_value4
}

eval_inverse_existence <- function(nb_mat){
  EV_inv <- c(1 / real_eigs(nb_mat),1)
  n_obs <- nrow(nb_mat)

  A_r <- function(r){
    Diagonal(n_obs)-(r*nb_mat)
  }

  traces <- trace_sequence(nb_mat,max_power = 10)

  eval_rcond_det <- function(r){
    mat_r <- as.matrix(A_r(r))
    data.frame("rho" = r,
               "det" = log(abs(det(mat_r))),
               "approx2_det"= lndetmc(r,traces[1:2],n_obs,model = "model_3"),
               "tapprox2_det" = det_approx_2(traces[1:2],r),
               "X0tapprox2_det" = det_Xapprox_2(traces[1:2],r,0),
               "X1tapprox2_det" = det_Xapprox_2(traces[1:2],r,1),
               "Xn1tapprox2_det" = det_Xapprox_2(traces[1:2],r,-1),
               "approx4_det"= lndetmc(r,traces[1:4],n_obs,model = "model_3"),
               "tapprox4_det" = det_approx_2(traces[1:4],r),
               "X0tapprox4_det" = det_Xapprox_2(traces[1:4],r,0),
               "X1tapprox4_det" = det_Xapprox_2(traces[1:4],r,1),
               "Xn1tapprox4_det" = det_Xapprox_2(traces[1:4],r,-1),
               "t4approx4_det" = det_approx_4(traces[1:4],r),
               "approx10_det"= lndetmc(r,traces,n_obs,model = "model_3"),
               "tapprox10_det" = det_approx_2(traces,r),
               "t4approx10_det" = det_approx_4(traces[1:4],r),
               "recip" = log(abs(rcond(mat_r))),
               "is_EV" = any(r == EV_inv))
  }

  rho <- seq(-3.5,3.5,by = 0.01) %>% c(EV_inv) %>% sort()
  det_conditions <- rho %>% lapply("eval_rcond_det") %>% lreduce("rbind")
  det_conditions
}


# see for Germany
A_ge_det_conditions <- eval_inverse_existence(nb_ge)
A_ge_det_conditions_gg <- A_ge_det_conditions %>% setDT() %>%
  melt(measure = c("det","recip"))

ggplot(A_ge_det_conditions_gg[between(rho,-2.5,3.5),],
       aes(x = rho, y = value, col = variable, size = (is_EV + 0.5),
           shape = is_EV)) +
  scale_shape_manual(values = c(1,24))+
  geom_line(mapping = aes(x = rho, y = value, col = variable), inherit.aes = FALSE) +
  geom_point()

A_ge_det_conditions_gg <- A_ge_det_conditions %>% setDT() %>%
  melt(measure = c("det","recip",
                   "approx2_det","approx4_det",#"approx10_det",
                   "tapprox2_det","tapprox4_det","tapprox10_det",
                   "t4approx4_det","t4approx10_det"))

ggplot(A_ge_det_conditions_gg[between(rho,-1.2,1.05),],
       aes(x = rho, y = value, col = variable, size = (is_EV + 0.5),
           shape = is_EV)) +
  scale_shape_manual(values = c(1,24))+
  geom_line(mapping = aes(x = rho, y = value, col = variable), inherit.aes = FALSE) +
  geom_point()


A_ge_det_conditions_gg <- A_ge_det_conditions %>% setDT() %>%
  melt(measure = c("det","recip",
                   # "approx2_det",
                   "approx4_det",
                   #"approx10_det",
                   # "tapprox2_det","tapprox4_det",#"tapprox10_det",
                   "X0tapprox2_det",
                   #"X1tapprox2_det",
                   #"Xn1tapprox2_det",
                   "X0tapprox4_det"#,
                 # "X1tapprox4_det",
                  #"Xn1tapprox4_det"
                  ))

ggplot(A_ge_det_conditions_gg[between(rho,-1.9,1.05),],
       aes(x = rho, y = value, col = variable, size = (is_EV + 0.5),
           shape = is_EV)) +
  scale_shape_manual(values = c(1,24))+
  geom_line(mapping = aes(x = rho, y = value, col = variable), inherit.aes = FALSE) +
  geom_point(na.rm = TRUE)



# see for USA
A_usa_det_conditions <- eval_inverse_existence(nb_usa)


A_usa_det_conditions_gg <- A_usa_det_conditions %>% setDT() %>%
  melt(measure = c("det","recip"))

ggplot(A_usa_det_conditions_gg[between(rho,-2.5,3.5),],
       aes(x = rho, y = value, col = variable, size = (is_EV + 0.5),
           shape = is_EV)) +
  scale_shape_manual(values = c(1,24))+
  geom_line(mapping = aes(x = rho, y = value, col = variable), inherit.aes = FALSE) +
  geom_point()

A_usa_det_conditions_gg <- A_usa_det_conditions %>% setDT() %>%
  melt(measure = c("det","recip",
                   "approx2_det","approx4_det","approx10_det",
                   "tapprox2_det","tapprox4_det","tapprox10_det",
                   "t4approx4_det","t4approx10_det"))


ggplot(A_usa_det_conditions_gg[between(rho,-1.03,1.02),],
       aes(x = rho, y = value, col = variable, size = (is_EV + 0.5),
           shape = is_EV)) +
  scale_shape_manual(values = c(1,24))+
  geom_line(mapping = aes(x = rho, y = value, col = variable), inherit.aes = FALSE) +
  geom_point()


