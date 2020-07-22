# maybe classes to allow empty slots
setClassUnion("maybe_data.table", c("NULL", "data.frame")) # data.table is no exported -> resort to data.frame
setClassUnion("maybe_Matrix"    , c("NULL", "Matrix"))
setClassUnion("maybe_list"      , c("NULL", "list"))
setClassUnion("maybe_numeric"   , c("NULL", "numeric"))


# ---- Generics ---------------------------------------------------------------

# ---- .count nodes accessors ----

#' @title Access the node_count of a network object
#' @param object One of; [sp_network()], [sp_network_pair()]
#' @name count
#' @export
setGenericVerif("count",function(object, ...){
  standardGeneric("count")})

# ---- .data accessors ----

#' @title Access the data of a network object
#' @param object One of; [sp_network()], [sp_network_pair()]
#' @name dat
#' @export
setGenericVerif("dat",  function(object, ...){
  standardGeneric("dat")})


#' @param value A data.frame to replace existing data
#' @rdname dat
#' @name dat<-
#' @export
setGenericVerif("dat<-",  function(object, ..., value){
  standardGeneric("dat<-")})


# ---- .id accessors ----

#' @title Access the id of a [sp_network()] a [sp_network_pair()] or
#'   a [sp_multi_network()]
#' @param object One of; [sp_network()], [sp_network_pair()],
#'   [sp_multi_network()]
#' @name id
#' @export
setGenericVerif("id", function(object, ...){
  standardGeneric("id")})

#' @param value A character replacing the existing id
#' @rdname id
#' @name id<-
#' @export
setGenericVerif("id<-",function(object,...,value){
  standardGeneric("id<-")})

# ---- .neighborhood accessors ----

#' @title Access the neighborhood of a [sp_network()]
#' @param object A [sp_network()]
#' @name neighborhood
#' @export
setGenericVerif("neighborhood", function(object){
  standardGeneric("neighborhood")})

#' @rdname neighborhood
#' @param value A neighborhood matrix to replace the existing one
#' @name neighborhood<-
#' @export
setGenericVerif("neighborhood<-", function(object, value){
  standardGeneric("neighborhood<-")})

#' @title Access the neighborhood of origin and destination networks in a
#'   [sp_network_pair()]
#' @param object A [sp_network_pair()]
#' @name neighborhoods
#' @export
setGenericVerif("neighborhoods",function(object, ...){
  standardGeneric("neighborhoods")})

# ---- .network data accessors----

#' @title Access the one or multiple [sp_network()] in a [sp_multi_network()]
#' @param object A [sp_multi_network()]
#' @name networks
#' @export
setGenericVerif("networks", function(object, ...){
  standardGeneric("networks")})

#' @title Access the one or multiple [sp_network_pair()] in a
#'   [sp_multi_network()]
#' @param object A [sp_multi_network()]
#' @name network_pairs
#' @export
setGenericVerif("network_pairs",function(object, ...){
  standardGeneric("network_pairs")})

# ---- .pair merge method ----
#' @title Create a long form data.frame of origin-destination pairs
#' @param object A [sp_multi_network()]
#' @name pair_merge
#' @export
setGenericVerif("pair_merge", function(object, ...){
  standardGeneric("pair_merge")})

# ---- .set columns method ----

#' @title Add columns to the data in a [sp_network()] or [sp_network_pair()]
#' @details  This function assigns by reference.
#' @param object One of; [sp_network()], [sp_network_pair()]
#' @name set_columns
#' @export
setGenericVerif("set_columns", function(object, ...){
  standardGeneric("set_columns")})


# ---- .variable names accessors ----

#' @title Access the column names of the data in a [sp_network()] or
#'   [sp_network_pair()]
#' @param object One of; [sp_network()], [sp_network_pair()]
#' @name variable_names
#' @export
setGenericVerif("variable_names", function(object,...){
  standardGeneric("variable_names")})

#' @rdname variable_names
#' @param value A character vector of new variable names
#' @name variable_names<-
#' @export
setGenericVerif("variable_names<-", function(object,...,value){
  standardGeneric("variable_names<-")})

