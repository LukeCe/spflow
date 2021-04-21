#' @include utils-OO.R spflow-package.R

# This script adds new generics and virtual classes that are used inside the
# package it should be run before all other classes which is ensured by the
# class_[abc] naming convention for scripts that define classes.


# ---- Maybe Classes ----------------------------------------------------------
# allow NULL slots during instantiation of classes
# data.table is not exported -> resort to data.frame
setClassUnion("maybe_data.frame", c("NULL", "data.frame"))
setClassUnion("maybe_Matrix"    , c("NULL", "Matrix"))
setClassUnion("maybe_matrix"    , c("NULL", "matrix"))
setClassUnion("maybe_any_matrix", c("NULL", "matrix", "Matrix"))
setClassUnion("maybe_list"      , c("NULL", "list"))
setClassUnion("maybe_numeric"   , c("NULL", "numeric"))


# ---- New Generics -----------------------------------------------------------

#' @rdname add_details
#' @name add_details
#' @keywords internal
setGenericVerif("add_details",function(object, ...){ # ---- add_details -------
  standardGeneric("add_details")})


#' @title Access the data of [sp network objects][sp_network_classes()]
#' @param object One of [sp_network_nodes-class()], [sp_network_pair-class()]
#' @name dat
#' @export
setGenericVerif("dat",  function(object, ...){ # ---- dat ---------------------
  standardGeneric("dat")})

#' @title Access the data of [sp network objects][sp_network_classes()]
#' @param value A data.frame to replace existing data
#' @name dat<-
#' @export
setGenericVerif("dat<-",  function(object, ..., value){ # ---- dat <- ---------
  standardGeneric("dat<-")})

#' @name dat_template
#' @rdname dat_template
#' @keywords internal
setGenericVerif("dat_template", function(object, ...){ # ---- dat_template ----
  standardGeneric("dat_template")})

#' @name drop_columns
#' @rdname drop_columns
#' @keywords internal
setGenericVerif("drop_columns", function(object, ...){ # ---- drop_columns ----
  standardGeneric("drop_columns")})


#' @title Access the id of [spflow network objects][sp_network_classes()]
#'
#' @param object A [spflow network object][sp_network_classes()]
#' @name id
#' @aliases id-set
#' @export
setGenericVerif("id", function(object, ...){ # ---- id ------------------------
  standardGeneric("id")})

#' @param value A character replacing the existing id
#' @rdname id
#' @name id<-
#' @export
setGenericVerif("id<-",function(object,...,value){ # ---- id <- ---------------
  standardGeneric("id<-")})

#' @name mcmc_results
#' @rdname spflow_model-class
#' @export
setGenericVerif("mcmc_results", function(object){ # ---- mcmc_results ---------
  standardGeneric("mcmc_results")})

#' @title Access the neighborhood of a [sp_network_nodes-class()]
#' @param object A [sp_network_nodes-class()]
#' @name neighborhood
#' @export
setGenericVerif("neighborhood", function(object){ # ---- neighborhood ---------
  standardGeneric("neighborhood")})

#' @rdname neighborhood
#' @param value A neighborhood matrix to replace the existing one
#' @name neighborhood<-
#' @export
setGenericVerif("neighborhood<-",
                function(object, value){ # ---- neighborhood <- ---------------
  standardGeneric("neighborhood<-")})

#' @title
#' Access the node count of [spflow network objects][sp_network_classes()]
#' @param object One of [sp_network_nodes-class()], [sp_network_pair-class()]
#' @rdname nnodes
#' @name nnodes
#' @export
setGenericVerif("nnodes",function(object, ...){ # ---- nnodes -----------------
  standardGeneric("nnodes")})

#' @title Access the pair count of a [sp_network_pair-class()]
#' @param object A [sp_network_pair-class()]
#' @rdname npairs
#' @name npairs
#' @export
#' @examples
#' pairs_ge_ge <- pull_pairs(multi_net_usa_ge, "ge_ge")
#' npairs(pairs_ge_ge)
setGenericVerif("npairs",function(object){ # ---- npairs ----------------------
  standardGeneric("npairs")})


#' @rdname pair_merge
#' @name pair_merge
#' @export
setGenericVerif("pair_merge", function(object, ...){ # ---- pair_merge --------
  standardGeneric("pair_merge")})


#' @title
#'   Access the neighborhood matrix of the nodes inside
#'   [spflow network objects][sp_network_classes()]
#' @param object A [sp_network_pair()]
#' @name pull_neighborhood
#' @rdname pull_neighborhood
#' @export
setGenericVerif("pull_neighborhood",
                function(object, ...){ # ---- pull_neighborhood ---------------
                  standardGeneric("pull_neighborhood")})

#' @title Access the one or multiple [sp_network_nodes()] in a
#'   [sp_multi_network()]
#' @param object A [sp_multi_network()]
#' @name pull_nodes
#' @export
setGenericVerif("pull_nodes",
                function(object, ...){ # ---- pull_nodes ----------------------
                  standardGeneric("pull_nodes")})

#' @title Access the one or multiple [sp_network_pair()] in a
#'   [sp_multi_network()]
#' @param object A [sp_multi_network()]
#' @name pull_pairs
#' @export
setGenericVerif("pull_pairs",
                function(object, ...){ # ---- pull_pairs ----------------------
                  standardGeneric("pull_pairs")})


#' @title
#' Access results of a [spflow_model()]
#'
#' @name results
#' @param object A [spflow_model()]
#' @export
setGenericVerif("results",  function(object){ # ---- results -------------
  standardGeneric("results")})

#' @title Replacement method for internal purposes
#' @param object A [spflow_model()]
#' @param value A data.frame of results
#' @name results<-
#' @keywords internal
setGenericVerif("results<-",  function(object, value){ # ---- results <- ------
  standardGeneric("results<-")})

#' @title Access results of a [spflow_model()] as a one row data.frame
#' @param object spflow_model_meta
#' @name results_flat
#' @rdname results_flat
#' @export
setGenericVerif("results_flat",
                function(object, ...){ # ---- results_flat --------------------
  standardGeneric("results_flat")})

#' @title Extract the standard deviation from a [spflow_model-class()]
#' @param object A [spflow_model-class()]
#' @rdname spflow_model
#' @name sd_error
setGenericVerif("sd_error", function(object){ # ---- sd_error -----------------
  standardGeneric("sd_error")})


#' @title Add columns to the data in a [sp_network_nodes()] or [sp_network_pair()]
#' @details  This function assigns by reference.
#' @param object One of; [sp_network_nodes()], [sp_network_pair()]
#' @name set_columns
#' @keywords internal
setGenericVerif("set_columns", function(object, ...){ # ---- set_columns ------
  standardGeneric("set_columns")})


#' @title Access variance-covariance matrix in a [spflow_model-class()]
#' @param object A [spflow_model-class()]
#' @name varcov
#' @export
setGenericVerif("varcov",  function(object){ # ---- varcov --------------------
  standardGeneric("varcov")})


#' @rdname variable_names
#' @name variable_names
#' @export
setGenericVerif("variable_names",
                function(object,...){ # ---- variable_names -------------------
  standardGeneric("variable_names")})

#' @rdname variable_names
#' @name variable_names<-
#' @keywords internal
setGenericVerif("variable_names<-",
                function(object,...,value){ # ---- variable_names <- ----------
  standardGeneric("variable_names<-")})

