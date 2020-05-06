# virtual classes to allow empty slots
setClassUnion("maybe_data.table", c("NULL", "data.frame")) # data.table is no exported -> resort to data.frame
setClassUnion("maybe_Matrix"    , c("NULL", "Matrix"))
setClassUnion("maybe_list"      , c("NULL", "list"))
setClassUnion("maybe_numeric"   , c("NULL", "numeric"))


# Generics
setGenericVerif("count",  function(object, ...){standardGeneric("count")})

setGenericVerif("dat",   function(object, ...){standardGeneric("dat")})
setGenericVerif("dat<-", function(object,value, ...){standardGeneric("dat<-")})

setGenericVerif("id",   function(object, ...){standardGeneric("id")})
setGenericVerif("id<-", function(object,value, ...){standardGeneric("id<-")})

setGenericVerif("interaction_data",   function(object, ...){standardGeneric("interaction_data")})

setGenericVerif("neighborhood",   function(object){standardGeneric("neighborhood")})
setGenericVerif("neighborhood<-", function(object,value){standardGeneric("neighborhood<-")})

setGenericVerif("variable_names",   function(object,...){standardGeneric("variable_names")})
setGenericVerif("variable_names<-", function(object,value,...){standardGeneric("variable_names<-")})

