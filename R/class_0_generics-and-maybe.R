#' @include utils.R spflow-package.R

# This script adds new generics and virtual classes that are used inside the
# package it should be run before all other classes which is ensured by the
# class_[abc] naming convention.


# ---- Maybe Classes ----------------------------------------------------------
# allow empty slots during instantiation
# data.table is no exported -> resort to data.frame
setClassUnion("maybe_data.table", c("NULL", "data.frame"))
setClassUnion("maybe_data.frame", c("NULL", "data.frame"))
setClassUnion("maybe_Matrix"    , c("NULL", "Matrix"))
setClassUnion("maybe_matrix"    , c("NULL", "matrix"))
setClassUnion("maybe_list"      , c("NULL", "list"))
setClassUnion("maybe_numeric"   , c("NULL", "numeric"))


# ---- New Generics -----------------------------------------------------------
#' @title Add details to a [spflow_model_meta()] object
#'
#' @details
#' The method adds the design matrix and the coefficient names to an
#' [spflow_model_meta()] object.
#' It also calculates the fitted values and the residuals as well as a
#' goodness-of-fit measure.
#'
#' @param object A [spflow_model_meta()] object
#' @name add_details
#' @export
setGenericVerif("add_details",function(object, ...){ # ---- add_details -------
  standardGeneric("add_details")})


#' @title Access the node_count of a network object
#' @param object One of; [sp_network_nodes()], [sp_network_pair()]
#' @name count
#' @export
setGenericVerif("count",function(object, ...){ # ---- count -------------------
  standardGeneric("count")})



#' @title Access the data of a network object
#' @param object One of; [sp_network_nodes()], [sp_network_pair()]
#' @name dat
#' @export
setGenericVerif("dat",  function(object, ...){ # ---- dat ---------------------
  standardGeneric("dat")})


#' @param value A data.frame to replace existing data
#' @rdname dat
#' @name dat<-
#' @export
setGenericVerif("dat<-",  function(object, ..., value){ # ---- dat <- ---------
  standardGeneric("dat<-")})

#' @title Access the schema of the data inside a netowrk object
#' @param object One of; [sp_network_nodes()], [sp_network_pair()]
#' @name dat_template
#' @keywords internal
setGenericVerif("dat_template", function(object, ...){ # ---- dat_template ----
  standardGeneric("dat_template")})

#' @title Drops columns from the data in a [sp_network_nodes()] or [sp_network_pair()]
#' @details  This function assigns by reference.
#' @param object One of; [sp_network_nodes()], [sp_network_pair()]
#' @name drop_columns
#' @export
setGenericVerif("drop_columns", function(object, ...){ # ---- drop_columns ----
  standardGeneric("drop_columns")})


#' @title Access the id of a [sp_network_nodes()] a [sp_network_pair()] or
#'   a [sp_multi_network()]
#' @param object One of; [sp_network_nodes()], [sp_network_pair()],
#'   [sp_multi_network()]
#' @name id
#' @export
setGenericVerif("id", function(object, ...){ # ---- id ------------------------
  standardGeneric("id")})

#' @param value A character replacing the existing id
#' @rdname id
#' @name id<-
#' @export
setGenericVerif("id<-",function(object,...,value){ # ---- id <- ---------------
  standardGeneric("id<-")})

#' @title Access the neighborhood of a [sp_network_nodes()]
#' @param object A [sp_network_nodes()]
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

#' @title Access the neighborhood of origin and destination networks in a
#'   [sp_network_pair()]
#' @param object A [sp_network_pair()]
#' @name neighborhoods
#' @export
setGenericVerif("neighborhoods",function(object, ...){
  standardGeneric("neighborhoods")})

#' @title Access the one or multiple [sp_network_nodes()] in a
#'   [sp_multi_network()]
#' @param object A [sp_multi_network()]
#' @name network_nodes
#' @export
setGenericVerif("network_nodes",
                function(object, ...){ # ---- network_nodes -------------------
  standardGeneric("network_nodes")})

#' @title Access the one or multiple [sp_network_pair()] in a
#'   [sp_multi_network()]
#' @param object A [sp_multi_network()]
#' @name network_pairs
#' @export
setGenericVerif("network_pairs",
                function(object, ...){ # ---- network_pairs -------------------
  standardGeneric("network_pairs")})

#' @title Create a long form data.frame of origin-destination pairs
#' @param object A [sp_multi_network()]
#' @name pair_merge
#' @export
setGenericVerif("pair_merge", function(object, ...){ # ---- pair_merge --------
  standardGeneric("pair_merge")})

#' @title Access results of an estimation
#' @param object spflow_model_meta
#' @name results
#' @rdname results
#' @export
setGenericVerif("results",  function(object){ # ---- results -------------
  standardGeneric("results")})

#' @param value replacement results
#' @name results
#' @rdname results
setGenericVerif("results<-",  function(object, value){ # ---- results <- --------
  standardGeneric("results<-")})

#' @title Add columns to the data in a [sp_network_nodes()] or [sp_network_pair()]
#' @details  This function assigns by reference.
#' @param object One of; [sp_network_nodes()], [sp_network_pair()]
#' @name set_columns
#' @keywords internal
setGenericVerif("set_columns", function(object, ...){ # ---- set_columns ------
  standardGeneric("set_columns")})


#' @title Access variance-covariance matrix of the estimators
#' @param object spflow_model_mle or spflow_model_s2sls
#' @name varcov
#' @export
setGenericVerif("varcov",  function(object){ # ---- varcov --------------------
  standardGeneric("varcov")})


#' @title Access the column names of the data in a [sp_network_nodes()] or
#'   [sp_network_pair()]
#' @param object One of; [sp_network_nodes()], [sp_network_pair()]
#' @name variable_names
#' @export
setGenericVerif("variable_names",
                function(object,...){ # ---- variable_names -------------------
  standardGeneric("variable_names")})

#' @rdname variable_names
#' @param value A character vector of new variable names
#' @name variable_names<-
#' @export
setGenericVerif("variable_names<-",
                function(object,...,value){ # ---- variable_names <- ----------
  standardGeneric("variable_names<-")})

