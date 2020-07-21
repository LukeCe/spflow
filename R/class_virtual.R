# maybe classes to allow empty slots
setClassUnion("maybe_data.table", c("NULL", "data.frame")) # data.table is no exported -> resort to data.frame
setClassUnion("maybe_Matrix"    , c("NULL", "Matrix"))
setClassUnion("maybe_list"      , c("NULL", "list"))
setClassUnion("maybe_numeric"   , c("NULL", "numeric"))


# ---- Generics ---------------------------------------------------------------

# ---- .count nodes accessors ----

#' @title Access the node_count of a network object
#' @name count
#' @export
setGenericVerif("count",function(object, ...){
  standardGeneric("count")})

# ---- .data accessors ----

#' @title Access the data of a network object
#' @name dat
#' @export
setGenericVerif("dat",  function(object, ...){
  standardGeneric("dat")})

#' @export
setGenericVerif("dat<-",  function(object,value, ...){
  standardGeneric("dat<-")})


# ---- .id accessors ----

#' @title Access the id of a network object
#' @name id
#' @export
setGenericVerif("id", function(object, ...){
  standardGeneric("id")})

#' @export
setGenericVerif("id<-",function(object,value, ...){
  standardGeneric("id<-")})

# ---- .neighborhood accessors ----

#' @title Access the neighborhood of a [sp_network()]
#' @name neighborhood
#' @export
setGenericVerif("neighborhood", function(object){
  standardGeneric("neighborhood")})

#' @export
setGenericVerif("neighborhood<-", function(object,value, ...){
  standardGeneric("neighborhood<-")})

#' @title
#' Access the neighborhood of origin and destination networks in a
#' [sp_network_pair()]
#' @name neighborhoods
#' @export
setGenericVerif("neighborhoods",function(object, ...){
  standardGeneric("neighborhoods")})

# ---- .network data accessors----

#' @title
#' Access the one or multiple [sp_network()] in a
#' [sp_multi_network()]
#' @name networks
#' @export
setGenericVerif("networks", function(object, ...){
  standardGeneric("networks")})

#' @title
#' Access the one or multiple [sp_network_pair()] in a
#' [sp_multi_network()]
#' @name network_pairs
#' @export
setGenericVerif("network_pairs",function(object, ...){
  standardGeneric("network_pairs")})

# ---- .pair merge method ----
#' @title Create a long form data.frame of origin-destination pairs
#' @name pair_merge
#' @export
setGenericVerif("pair_merge", function(object, ...){
  standardGeneric("pair_merge")})

# ---- .set columns method ----

#' @title Add columns to the data inside a [sp_network()] or
#' [sp_network_pair()].
#' @name set_columns
#' @export
setGenericVerif("set_columns", function(object, ...){
  standardGeneric("set_columns")})


# ---- .variable names accessors ----

#' @title
#' Access the column names of the data inside a [sp_network()] or
#' [sp_network_pair()].
#' @name variable_names
#' @export
setGenericVerif("variable_names", function(object,...){
  standardGeneric("variable_names")})

#' @export
setGenericVerif("variable_names<-", function(object,value,...){
  standardGeneric("variable_names<-")})

