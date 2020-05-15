# virtual classes to allow empty slots
setClassUnion("maybe_data.table", c("NULL", "data.frame")) # data.table is no exported -> resort to data.frame
setClassUnion("maybe_Matrix"    , c("NULL", "Matrix"))
setClassUnion("maybe_list"      , c("NULL", "list"))
setClassUnion("maybe_numeric"   , c("NULL", "numeric"))


# Generics
setGenericVerif(
  "count",
  function(object, ...){standardGeneric("count")})

setGenericVerif(
  "dat",
  function(object, ...){standardGeneric("dat")})
setGenericVerif(
  "dat<-",
  function(object,value, ...){standardGeneric("dat<-")})

#' @export
setGenericVerif(
  "id",
  function(object, ...){standardGeneric("id")})
setGenericVerif(
  "id<-",
  function(object,value, ...){standardGeneric("id<-")})

setGenericVerif(
  "interaction_data",
  function(object, ...){standardGeneric("interaction_data")})

setGenericVerif(
  "pair_merge",
  function(object, ...){standardGeneric("pair_merge")})

setGenericVerif(
  "neighborhood",
  function(object){standardGeneric("neighborhood")})

setGenericVerif(
  "neighborhood<-",
  function(object,value){standardGeneric("neighborhood<-")})

setGenericVerif(
  "neighborhoods",
  function(object, ...){standardGeneric("neighborhoods")})

setGenericVerif(
  "networks",
  function(object, ...){standardGeneric("networks")})

setGenericVerif(
  "network_pairs",
  function(object, ...){standardGeneric("network_pairs")})

setGenericVerif(
  "set_columns",
  function(object, ...){standardGeneric("set_columns")})

setGenericVerif(
  "variable_names",
  function(object,...){standardGeneric("variable_names")})

setGenericVerif(
  "variable_names<-",
  function(object,value,...){standardGeneric("variable_names<-")})

