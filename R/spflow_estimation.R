#' @keywords internal
spflow_estimation <- function(
  spflow_moments,
  spflow_nbfunctions,
  estimation_control) {

  estimation_results <- switch(estimation_control[["estimation_method"]],
    "ols" = {
      spflow_ols(
        ZZ  = spflow_moments[["ZZ"]],
        ZY  = spflow_moments[["ZY"]],
        TSS = spflow_moments[["TSS"]],
        N   = spflow_moments[["N"]],
        TCORR = spflow_moments[["TCORR"]],
        estimation_control = estimation_control
      )},
    "s2sls" = {
      spflow_s2sls(
        UU  = spflow_moments[["UU"]],
        UY  = spflow_moments[["UY"]],
        ZZ  = spflow_moments[["ZZ"]],
        ZY  = spflow_moments[["ZY"]],
        TSS = spflow_moments[["TSS"]],
        N   = spflow_moments[["N"]],
        TCORR = spflow_moments[["TCORR"]],
        pspace_validator = spflow_nbfunctions[["pspace_validator"]],
        estimation_control = estimation_control
        )},
    "mle" = {
      spflow_mle(
        ZZ    = spflow_moments[["ZZ"]],
        ZY    = spflow_moments[["ZY"]],
        TSS   = spflow_moments[["TSS"]],
        N     = spflow_moments[["N"]],
        n_d   = spflow_moments[["n_d"]],
        n_o   = spflow_moments[["n_o"]],
        TCORR = spflow_moments[["TCORR"]],
        estimation_control = estimation_control,
        pspace_validator = spflow_nbfunctions[["pspace_validator"]],
        logdet_calculator = spflow_nbfunctions[["logdet_calculator"]]
      )},
    "mcmc" = {
      spflow_mcmc(
        ZZ  = spflow_moments[["ZZ"]],
        ZY  = spflow_moments[["ZY"]],
        TSS = spflow_moments[["TSS"]],
        N   = spflow_moments[["N"]],
        n_d = spflow_moments[["n_d"]],
        n_o = spflow_moments[["n_o"]],
        TCORR = spflow_moments[["TCORR"]],
        estimation_control = estimation_control,
        pspace_validator = spflow_nbfunctions[["pspace_validator"]],
        logdet_calculator = spflow_nbfunctions[["logdet_calculator"]]
      )})

  estimation_results@spflow_nbfunctions <- spflow_nbfunctions
  return(estimation_results)
}

#' @title Internal method to add details to a [spflow_model-class()]
#'
#' @details
#' The method adds the design matrix and the coefficient names to an
#' [spflow_model-class()] object.
#' It also calculates the fitted values and the residuals as well as a
#' goodness-of-fit measure.
#'
#' @return An object of class spflow_model
#' @noRd
#' @keywords  internal
spflow_post_estimation <- function(
    object,
    spflow_networks,
    spflow_indicators,
    spflow_moments,
    spflow_matrices,
    spflow_formula) {


  object@spflow_moments <- spflow_moments
  object@spflow_formula <- spflow_formula
  object@spflow_matrices <- drop_instruments(spflow_matrices)
  object@spflow_networks <- spflow_networks

  object@spflow_indicators <- cbind(spflow_indicators, SIGNAL = NA, FITTED = NA)
  pred <- predict(
    object,
    method = object@estimation_control[["fitted_value_method"]],
    add_new_signal = TRUE)
  object@spflow_indicators[c("SIGNAL","FITTED")] <- pred[c("NEW_SIGNAL", "PREDICTION")]
  object@estimation_diagnostics[["R2_corr"]] <-
    spflow_indicators2Rcorr(object@spflow_indicators)

  object@estimation_diagnostics <- c(
    object@estimation_diagnostics,
    spflow_indicators2obs(spflow_indicators))

  if (isTRUE(object@estimation_control[["reduce_model_size"]])) {
    object@spflow_matrices <- NULL
    object@spflow_networks <- NULL
    object@spflow_indicators <- NULL
  }

  return(object)
}


#' @importFrom utils askYesNo
#' @keywords internal
solve_savely <- function(ZZ, ZY, TCORR, error_msg) {

  result <- try(solve(ZZ, ZY), silent = TRUE)
  if (!is(result,"try-error"))
    return(result)

  if (interactive()) {
    question <- sprintfwrap("
    The covariates of your models lead to a singular fit! <br>
    Type \"yes\" to show a correlpation plot.")
    answer <- askYesNo(question, default = FALSE)
    if (answer)
      cor_image(TCORR)
  }

  if (missing(error_msg))
    stop(result[1])

  stop(sprintfwrap(error_msg))
}


#' @export
spflow_refit <- function(
    object,
    refit_type = "ar_family",
    global_vars = c("model_coherence", "R2_corr", "ll", "AIC", "N_sample"),
    sample_weights = NULL,
    protected_params = "(Intercept)") {


  refit_options <- c("ar_family", "stepwise", "samples")
  assert_valid_option(refit_type,refit_options)

  if (refit_type == "ar_family")
    models <- spflow_refit_ar_family(object)

  if (refit_type == "stepwise")
    models <- spflow_refit_stepwise(object, protected_params)

  if (refit_type == "samples") {
    if (object@estimation_control[["estimation_method"]] == "s2sls")
      object@spflow_matrices <- derive_spflow_matrices(
        id_spflow_pairs = id(object@spflow_networks)[["pairs"]],
        spflow_networks = object@spflow_networks,
        spflow_formula = object@spflow_formula,
        spflow_control = object@estimation_control,
        na_rm = object@estimation_control[["na_rm"]])
    models <- spflow_refit_samples(object, sample_weights)
  }

  return(compare_results(models))
}


#' @keywords internal
spflow_refit_ar_family <- function(object) {

  # functions
  get_obs_ylags <- function(mat_list) {
    obs_Y <- Reduce("|",lapply(mat_list, is.na))
    obs_Y <- !obs_Y[pair_index]
    if (all(obs_Y))
      obs_Y <- NULL
    obs_Y
  }
  estimate_with_new_Y <- function(...) tryCatch(estimate_with_new_Y2(...), error = function(e) NULL)
  estimate_with_new_Y2 <- function(Y_, new_control, new_mom, new_mom_emp) {

    new_indic <- indic9
    new_indic[["HAS_ZY"]] <- get_obs_ylags(Y_)
    new_obs <- spflow_indicators2obs(new_indic)
    stable_sample <- identical(sample9, get_sample(new_indic))
    if (stable_sample) {
      new_mod <- as.integer(substr(new_control[["model"]],7,7))
      new_mom <- subset_m9_moments(new_mom, new_mod)
      new_mom_emp <- subset_m9_moments(new_mom_emp, new_mod)
    } else {
      new_mat <- mat9
      new_mat[["Y_"]] <- lapply(Y_, drop_na)
      new_mom <- derive_spflow_moments(
        spflow_matrices = new_mat,
        n_o = new_obs[["N_orig"]],
        n_d = new_obs[["N_dest"]],
        N = new_obs[["N_sample"]],
        wt = spflow_indicators2wtmat(new_indic),
        na_rm = new_control[["na_rm"]])
      new_mom_emp <- derive_empric_moments(
        mat = new_mat,
        dg = new_obs,
        mom = new_mom,
        indic = new_indic,
        ctrl = new_control)
    }

    new_nb <- derive_spflow_nbfunctions(
      OW = OW, DW = DW,
      estimation_control =  new_control,
      spflow_indicators =  new_indic)
    new_res <- spflow_estimation(new_mom, new_nb, new_control)
    new_dg <- new_obs
    new_dg[["R2_corr"]] <- mom2Rcorr(new_mom_emp, coef(new_res))
    new_res@estimation_diagnostics[names(new_dg)] <- new_dg
    return(new_res)
  }
  subset_m9_moments <- function(mom9, new_model) {

    mom <- mom9
    if (new_model %in% c(1:4,7)) {
      sel <- list("1" = 1, "2" = c(1,2), "3" = c(1,3), "4" = c(1,4), "7" = c(1,2,3))
      sel <- sel[[as.character(new_model)]]
      mom[["TSS"]] <- mom[["TSS"]][sel,sel, drop = FALSE]
      mom[["ZY"]] <- mom[["ZY"]][,sel, drop = FALSE]
      mom[["UY"]] <- mom[["UY"]][,sel, drop = FALSE]
      mom[["sumY"]] <- mom[["sumY"]][sel]
    }

    if (new_model %in% c(5,6)) {
      cc <- list("5" = matrix(c(1,0,0,0,0,0,1/2,1/2),ncol = 4, byrow = TRUE),
                 "6" = matrix(c(1,0,0,0,0,1/3,1/3,1/3),ncol = 4, byrow = TRUE))
      cc <- cc[[as.character(new_model)]]
      mom[["TSS"]] <- cc %*% mom[["TSS"]] %*% t(cc)
      mom[["ZY"]] <- mom[["ZY"]] %*% t(cc)
      mom[["UY"]] <- mom[["UY"]] %*% t(cc)
      mom[["sumY"]] <- mom[["sumY"]] %|!|% (cc %*% mom[["sumY"]])
    }
    return(mom)

  }

  # check which model is already given
  model <- as.integer(substr(object@estimation_control[["model"]],7,7))
  res <- vector("list",length = 9)
  names(res) <- sprintf("Model (%s)", 1:9)
  res[[model]] <- object

  # start with model 9 that nests the whole family
  control9 <- object@estimation_control
  control9[["model"]] <- "model_9"
  control9[["reduce_model_size"]] <- TRUE
  control9[["fitted_value_method"]] <- "TS"
  if (control9[["estimation_method"]] == "ols") {
    control9[["estimation_method"]] <- "mle"
    control9[["approx_expectation"]] <- TRUE
    control9[["expectation_approx_order"]] <- 10
    control9[["logdet_approx_order"]] <- 2
    control9[["mle_optim_limit"]] <- 100
    control9[["mle_hessian_method"]] <- "mixed"
  }

  control9 <- control9[names(control9)]




  indic9 <- object@spflow_indicators
  pair_index <- spflow_indicators2pairindex(indic9)

  mom9 <- object@spflow_moments
  mat9 <- object@spflow_matrices
  obs9 <- object@estimation_diagnostics
  Y9_ <- mat9[["Y_"]]
  OW <- neighborhood(object, "OW")
  DW <- neighborhood(object, "DW")


  Y1 <- spflow_indicators2mat(indic9,do_values = "ACTUAL")
  missing_flows <- any(is.na(Y1))
  missing_lags <- model < 8
  if (missing_flows | missing_lags) {
    if (missing_flows)
      Y9_[[1]] <- Y1
    Y9_ <- lag_flow_matrix(
      Y = Y1,
      model = control9[["model"]],
      OW = OW, DW = DW,
      name = names(Y9_)[1],
      M_indicator = spflow_indicators2mat(indic9))

    indic9[["HAS_ZY"]] <- get_obs_ylags(Y9_)
    mat9[["Y_"]] <- lapply(Y9_, drop_na)
    obs9 <- spflow_indicators2obs(indic9)
    mom9 <- derive_spflow_moments(
      spflow_matrices = mat9,
      n_o = obs9[["N_orig"]],
      n_d = obs9[["N_dest"]],
      N = obs9[["N_sample"]],
      wt = spflow_indicators2wtmat(indic9),
      na_rm = control9[["na_rm"]])
  }

  sample9 <- get_sample(indic9)
  mom_emp9 <- derive_empric_moments(
    mat = mat9,
    dg = obs9,
    mom = mom9,
    indic = indic9,
    ctrl = control9)



  # estimate the 9 submodels
  if (model != 9) {
    res[[9]] <- estimate_with_new_Y(
      Y_ = Y9_,
      new_control = control9,
      new_mom = mom9,
      new_mom_emp = mom_emp9)
  }

  if (model != 8) {
    control9[["model"]] <- "model_8"
    res[[8]] <- estimate_with_new_Y(
      Y_ = Y9_,
      new_control = control9,
      new_mom = mom9,
      new_mom_emp = mom_emp9)
  }

  if (model != 7) {
    control9[["model"]] <- "model_7"
    res[[7]] <- estimate_with_new_Y(
      Y_ = Y9_[1:3],
      new_control = control9,
      new_mom = mom9,
      new_mom_emp = mom_emp9)
  }

  if (model != 6) {
    control9[["model"]] <- "model_6"
    Y_ <- c(Y9_[1], Reduce("+", Y9_[2:4])/3)
    names(Y_) <- paste0(names(Y9_)[1],c("",".odw"))
    res[[6]] <- estimate_with_new_Y(
      Y_ = Y_,
      new_control = control9,
      new_mom = mom9,
      new_mom_emp = mom_emp9)
  }

  if (model != 5) {
    control9[["model"]] <- "model_5"
    Y_ <- c(Y9_[1], Reduce("+", Y9_[2:3])/2)
    names(Y_) <- paste0(names(Y9_)[1],c("",".od"))
    res[[5]] <- estimate_with_new_Y(
      Y_ = Y_,
      new_control = control9,
      new_mom = mom9,
      new_mom_emp = mom_emp9)
  }

  if (model != 4) {
    control9[["model"]] <- "model_4"
    res[[4]] <- estimate_with_new_Y(
      Y_ = Y9_[c(1,4)],
      new_control = control9,
      new_mom = mom9,
      new_mom_emp = mom_emp9)
  }

  if (model != 3) {
    control9[["model"]] <- "model_3"
    res[[3]] <- estimate_with_new_Y(
      Y_ = Y9_[c(1,3)],
      new_control = control9,
      new_mom = mom9,
      new_mom_emp = mom_emp9)
  }

  if (model != 2) {
    control9[["model"]] <- "model_2"
    res[[2]] <- estimate_with_new_Y(
      Y_ = Y9_[c(1,2)],
      new_control = control9,
      new_mom = mom9,
      new_mom_emp = mom_emp9)
  }

  if (model != 1) {
    control9[["model"]] <- "model_1"
    control9[["estimation_method"]] <- "ols"

    res[[1]] <- estimate_with_new_Y(
      Y_ = Y9_[1],
      new_control = control9,
      new_mom = mom9,
      new_mom_emp = mom_emp9)
  }

  return(res[seq(length(res),1)])
}

#' @keywords internal
spflow_refit_samples <- function(
    object,
    new_weights) {

  # solve naming
  nwt <- names(new_weights)
  names(new_weights) <- sprintf("Sample (%s)", seq(length(new_weights)))
  if (!is.null(nwt))
    names(new_weights)[nwt != ""] <- nwt[nwt != ""]

  old_weights <- object@spflow_indicators[["WEIGHT"]]
  new_weights <- lapply(
    new_weights, "derive_spflow_weights",
    spflow_data = pull_spflow_data(object@spflow_networks),
    do_indexes = sapply(object@spflow_indicators, as.integer))


  res <- vector("list", length = length(new_weights) + 1)
  names(res) <- c("Original", names(new_weights))
  res[["Original"]] <- object
  estimate_with_new_weights <- function(...) tryCatch(estimate_with_new_weights2(...), error = function(e) NULL)
  estimate_with_new_weights2 <- function(object, new_wt) {


    object@spflow_indicators[["WEIGHT"]] <- new_wt
    new_wt <- spflow_indicators2wtmat(object@spflow_indicators)
    n_o <- nobs(object, "orig")
    n_d <- nobs(object, "dest")
    N <- if (is.null(new_wt)) n_o * n_d else nnzero(new_wt)


    new_mom <- derive_spflow_moments(
      spflow_matrices = object@spflow_matrices,
      n_o = n_o, n_d = n_d, N = N,
      wt = spflow_indicators2wtmat(object@spflow_indicators),
      na_rm = object@estimation_control[["na_rm"]])

    new_res <- spflow_estimation(new_mom,object@spflow_nbfunctions,object@estimation_control)
    new_dg <- spflow_indicators2obs(object@spflow_indicators)

    new_mom_emp <- derive_empric_moments(object, dg = new_dg, mom = new_mom)
    new_dg[["R2_corr"]] <- mom2Rcorr(new_mom_emp, coef(new_res))
    new_res@estimation_diagnostics[names(new_dg)] <- new_dg
    return(new_res)
  }

  # go backwards to handle NULL cases on the fly
  s <- length(new_weights)
  p <- s + 1
  for (i in seq_len(s))
    res[[p - i + 1]] <- estimate_with_new_weights(object, new_weights[[p - i]])

  return(res)
}


#' @keywords internal
spflow_refit_stepwise <- function(object, protected_params = NULL) {

  # extract constant parts
  ctrl <- object@estimation_control
  ctrl[["reduce_model_size"]] <- TRUE
  ctrl[["fitted_value_method"]] <- "TS"
  dg <- object@estimation_diagnostics

  mom <- object@spflow_moments
  mom_emp <- derive_empric_moments(object)
  nbfunctions <- object@spflow_nbfunctions

  # pre assign result and fill the first
  n_steps <- 1 - sum(names(coef(object, "delta")) %in% protected_params)
  n_steps <- length(coef(object, "delta")) + min(0,n_steps)
  res <- vector("list", n_steps)
  names(res) <- sprintf("Drop (%s)", seq(0,n_steps-1))
  res[[1]] <- object
  if (object@estimation_control[["fitted_value_method"]] != "TS")
    res[[1]]@estimation_diagnostics[["R2_corr"]] <- mom2Rcorr(mom_emp,coef(object))

  for (i in seq_len(n_steps - 1)) {
    res_delta <- results(res[[i]])[names(coef(res[[i]], "delta")),,drop = FALSE]
    drop_delta <- rank(res_delta[["p.val"]])
    drop_delta <- drop_delta * (!rownames(res_delta) %in% protected_params)
    drop_delta <- which.max(drop_delta)

    mom$ZZ <- mom$ZZ[-drop_delta,-drop_delta, drop = FALSE]
    mom$UU <- mom$UU[-drop_delta,-drop_delta, drop = FALSE]
    mom$ZY <- mom$ZY[-drop_delta,, drop = FALSE]
    mom$UY <- mom$UY[-drop_delta,, drop = FALSE]

    mom_emp$ZZ <- mom_emp$ZZ[-drop_delta,-drop_delta, drop = FALSE]
    mom_emp$UU <- mom_emp$UU[-drop_delta,-drop_delta, drop = FALSE]
    mom_emp$ZY <- mom_emp$ZY[-drop_delta,, drop = FALSE]
    mom_emp$UY <- mom_emp$UY[-drop_delta,, drop = FALSE]
    mom_emp$sumZ <- mom_emp$sumZ[-drop_delta]


    new_res <- spflow_estimation(mom, nbfunctions, ctrl)
    new_dg <- new_res@estimation_diagnostics
    new_dg[["R2_corr"]] <- mom2Rcorr(mom_emp,coef(new_res))
    dg[names(new_dg)] <- new_dg
    new_res@estimation_diagnostics <- dg
    res[[i + 1]] <- new_res
  }

  return(res)
}

#' @keywords internal
derive_empric_moments <- function(
    object,
    indic = object@spflow_indicators,
    mat = object@spflow_matrices,
    mom = object@spflow_moments,
    dg = object@estimation_diagnostics,
    ctrl = object@estimation_control) {

  if (is.numeric(indic[["WEIGHT"]])) {
    mom <- derive_spflow_moments(
      spflow_matrices = mat,
      n_o = dg[["N_orig"]],
      n_d = dg[["N_dest"]],
      N = dg[["N_sample"]],
      wt =  spflow_indicators2wtmat(indic, as_binary = TRUE))
  }


  mom_sums <- list("sumY" = mom[["ZY"]][1,], "sumZ" =  mom[["ZZ"]][1,])
  if (!"(Intercept)" %in% colnames(mom[["ZZ"]])) {
    wt <- spflow_indicators2wtmat(indic, as_binary = TRUE)
    cI <- (Diagonal(dg[["N_orig"]]) * wt) %T% ctrl[["use_intra"]]
    mom_sums <- list(
      "sumY" = sapply(mat[["Y_"]], function(x) sum(x * wt)),
      "sumZ" = c(var_block_alpha(wt),
                 var_block_alpha_alpha_I(cI),
                 var_block_alpha_beta(mat[c("D_","O_","I_")], derive_weights_DOI(wt)),
                 var_block_alpha_gamma(lapply(mat[["P_"]], "*", wt))))
  }

  return(c(mom,mom_sums))
}

#' @keywords internal
mom2Rcorr <- function(mom, mu) {

  ZZ <- mom[["ZZ"]]
  ZY <- mom[["ZY"]][,1,drop=FALSE]

  JJ <- mom[["TSS"]][-1,-1] %||% NULL
  ZJ <- mom[["ZY"]][,-1,drop=FALSE] %||% NULL
  t_ZJ <- ZJ %|!|% t
  YJ <- mom[["TSS"]][-1,1,drop=FALSE] %||% NULL

  N <- mom[["N"]]
  sumY <- mom[["sumY"]]
  sumZ <- mom[["sumZ"]]
  sumYhat <- as.numeric(mu %*% c(sumY[-1] %||% NULL, sumZ))
  prodYYhat <- as.numeric(mu %*% rbind(YJ,ZY))
  num <- N * prodYYhat - sumYhat * sumY[1]

  sqsumY <- mom[["TSS"]][1,1]
  sqsumYhat <- mu %*% rbind(cbind(JJ,t_ZJ),cbind(ZJ,ZZ)) %*% mu
  denum <- sqrt(N * sqsumY - sumY[1]^2) * sqrt(N * sqsumYhat - sumYhat^2)
  return(as.numeric((num/denum)^2))
}

