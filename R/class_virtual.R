# virtual classes to allow empty slots
setClassUnion("maybe_Matrix"    , c("NULL","Matrix"))
setClassUnion("maybe_list"      , c("NULL","list"))
setClassUnion("maybe_numeric"   , c("NULL","numeric"))
setClassUnion("maybe_data.frame", c("NULL","data.frame"))
