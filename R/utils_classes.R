#' @keywords internal
safely_to_list <- function(obj) {
  if (is.list(obj)) return(obj)
  else return(list(obj))
}

#' @keywords internal
setGenericVerif <- function(x,y) {
  if ( !isGeneric(x))  setGeneric(x,y)
}

#' @keywords internal
savely_as <- function(obj, class, ...) {
  if (is.null(obj))
    return(NULL)

  return(as(obj,class, ...))
}

#' @keywords internal
try_coercion <- function(obj,class) {

  obj_as_class <- try(savely_as(obj,class),silent = TRUE)

  assert(!"try-error" %in% class(obj_as_class),
         sprintf("Object [%s] must be coercible to a [%s]!",
                 deparse(substitute(obj,parent.frame())),
                 class))

  return(obj_as_class)
}
