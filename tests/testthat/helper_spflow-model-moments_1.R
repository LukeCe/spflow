# ---- case 1 -----------------------------------------------------------------
# weighted and non-weighted moments for symmetric flows

n <- net_dat_letters %>% nrow()
letters_model_matrices <- list(
  "Y_" = list("Y" = matrix(pair_dat_letters$YY,n,n),
              "Y_o" = matrix(pair_dat_letters$YY * 2,n,n)),
  "G_" = list("G" = matrix(pair_dat_letters$GG,n,n),
              "G.lag1" = matrix(pair_dat_letters$GG * 4,n,n)),
  "D_" = as.matrix(net_dat_letters[,"XX"]),
  "O_" = as.matrix(net_dat_letters[,"XX"]),
  "I_" = as.matrix(net_dat_letters[,"XX"]),
  "constants" = list("global" = 1, "intra" = list("In" = Diagonal(n))),
  "W_" = list("OW" = sp_net_letters %>% neighborhood()),
  "wt" = matrix(1/abs(pair_dat_letters$YY),n,n)
)

iota_n <- rep(1,n)
iota_I <- as.vector(diag(n))
vec_form_letters <- cbind(
  letters_model_matrices$Y_$Y %>% as.vector(),
  letters_model_matrices$Y_$Y_o %>% as.vector(),
  iota_n %x% iota_n,  # const
  iota_I, # const_intra
  iota_n %x% letters_model_matrices$D_,
  letters_model_matrices$D_ %x% iota_n,
  (letters_model_matrices$D_ %x% iota_n) * iota_I,
  letters_model_matrices$G$G %>% as.vector(),
  letters_model_matrices$G$G.lag1 %>% as.vector())

flow_dims <- list(  "N" = n^2, "n_d" = n, "n_o" = n)
moments_letters <- crossprod(vec_form_letters)
moments_letters <- list(
  "TSS" = moments_letters[ (1:2), (1:2)],
  "HH"  = moments_letters[-(1:2),-(1:2)],
  "HY"  = moments_letters[-(1:2), (1:2)]) %>%
  c(flow_dims)

vec_letters_wt <- letters_model_matrices$wt %>% as.vector() %>% sqrt()
moments_letters_wt <- crossprod(vec_form_letters * vec_letters_wt)
moments_letters_wt <- list(
  "TSS" = moments_letters_wt[ (1:2), (1:2)],
  "HH"  = moments_letters_wt[-(1:2),-(1:2)],
  "HY"  = moments_letters_wt[-(1:2), (1:2)]) %>%
  c(flow_dims)

drop_objects <- c("n","vec_letters_wt","iota_n","iota_I","flow_dims")
rm(list=drop_objects)
