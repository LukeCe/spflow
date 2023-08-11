#' @title Compute spatial impacts matrix
#'
#' @description
#' Marginal effect in spatial econometric models are observation dependent.
#' Here we compute the changes to all observations that follow from changes in
#' a one variable for one (or multiple) observations.
#' Here, these effects are represented as a matrix with the same dimension as the
#' dependent variable.
#'
#'
#' @param object an [spflow_model-class()]
#' @param ... arguments passed on to [predict_effect()]
#' @param change_net a character of length one, identifying one of the
#'   [spflow_network_classes()] where changes to the data should be applied
#' @param change_var a character of length one, identifying one variable
#'   The character musts refer to the variable and its name to the id of
#' @param change_obs A numeric that identifies the observation(s) to change (by position)
#' @param x_is_log A logical, if `TRUE` the independent variable is assumed to
#'   be in logs and the effects are computed for a relative change of the form
#'   `x_new = 1.01 * x_old`.
#'   If `FALSE` the effects are computed for a unit increase of the form
#'   `x_new = 1 + x_old`.
#'
#' @return A Matrix
#' @export
#'
#' @author Lukas Dargel
#' @seealso predict_effect
#' @examples
#'
#' model9 <- spflow(
#' spflow_formula = y9 ~ . + P_(DISTANCE),
#' spflow_networks = multi_net_usa_ge,
#' id_net_pair = "ge_ge")
#'
#' # effect matrix, summary and decomposition
#' MI <- impacts_matrix(model9,change_net = "ge",change_var =  "X", change_obs = 8)
#' total_effect <- sum(MI)
#' intra_effect <- MI[8,8]
#' origi_effect <- sum(MI[,8]) - intra_effect
#' desti_effect <- sum(MI[8,]) - intra_effect
#' netwo_effect <- sum(MI[-8,-8])
#' total_effect == (intra_effect + origi_effect + desti_effect + netwo_effect)
#' @name impacts_matrix
#' @aliases impacts_matrix
#' @rdname impacts_matrix
#' @export
setMethod(
  f = "impacts_matrix",
  signature = "spflow_model",
  function(object,
           ...,
           change_net,
           change_var,
           change_obs = 1L,
           x_is_log   = FALSE) {

    assert_valid_option(change_net, c2(id(object@spflow_networks)))
    assert_is_single_x(change_var, "character")
    assert_is(change_obs, "numeric")
    assert_is_single_x(x_is_log, "logical")

    source_dat <- dat(object@spflow_networks, change_net)
    quant_cols <- setdiff(names(source_dat), get_keycols(source_dat))
    assert(change_var %in% names(source_dat),
           "The change_var for network \"%s\" musst be on of %s!",
           change_net, deparse(quant_cols))


    assert(all(change_obs %in% seq(nrow(source_dat))),
           "The change_obs musst corerspond to integers in 1,...,%s!", nrow(source_dat))

    change_dat <- source_dat[change_obs, c(get_keycols(source_dat,no_coords = TRUE), change_var)]
    change_dat[[2]] <- if (x_is_log) change_dat[[2]] * 1.01 else change_dat[[2]] + 1
    change_dat <- named_list(change_net, change_dat)
    result <- predict_effect(object, change_dat, return_type = "M", ...)

    attr(result, "change_dat") <- change_dat
    return(result)
  })
