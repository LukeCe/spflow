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

eval_inverse_existence <- function(nb_mat){
  EV_inv <- c(1 / real_eigs(nb_mat),1)
  A_r <- function(r){
    Diagonal(nrow(nb_mat))-(r*nb_mat)
  }
  eval_rcond_det <- function(r){
    mat_r <- as.matrix(A_r(r))
    data.frame("rho" = r, "det" = abs(det(mat_r)), "recip" = rcond(mat_r),
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

ggplot(A_ge_det_conditions_gg[between(rho,-2,2.5),],
       aes(x = rho, y = log(abs(value)), col = variable, size = (is_EV + 0.5),
           shape = is_EV)) +
  scale_shape_manual(values = c(1,24))+
  geom_line(mapping = aes(x = rho, y = log(abs(value)), col = variable), inherit.aes = FALSE) +
  geom_point()


# see for USA
A_usa_det_conditions <- eval_inverse_existence(nb_usa)
A_usa_det_conditions_gg <- A_usa_det_conditions %>% setDT() %>%
  melt(measure = c("det","recip"))

ggplot(A_usa_det_conditions_gg[between(rho,-2.5,2.5),],
       aes(x = rho, y = log(abs(value)), col = variable, size = (is_EV + 0.5),
           shape = is_EV)) +
  scale_shape_manual(values = c(1,24))+
  geom_line(mapping = aes(x = rho, y = log(abs(value)), col = variable), inherit.aes = FALSE) +
  geom_point()


