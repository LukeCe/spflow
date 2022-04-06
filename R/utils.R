#' @keywords internal
factor_in_order <- function(x) {
  factor(x,levels = as.character(unique(x)))
}

#' @keywords internal
sequentialize_index <- function(index_list) {
  len <- unlist(lapply(index_list, length))
  shift <- cumsum(c(0,len))[1:length(index_list)]
  Map("+", index_list, as.list(shift))
}

#' @keywords internal
prefix_columns <- function(obj,prefix){
  `colnames<-`(obj, paste0(prefix, colnames(obj)))
}

#' @keywords internal
suffix_columns <- function(obj,suffix){
  `colnames<-`(obj, paste0(colnames(obj), suffix))
}

#' @title Simple computation of the great circle distance
#' @details Unit is kilometer
#' @export
haversine_distance <- function(lon1, lat1, lon2, lat2) {
  lon1 <- degree2radian(lon1)
  lat1 <- degree2radian(lat1)
  lat2 <- degree2radian(lat2)
  lon2 <- degree2radian(lon2)

  diff_lon <- lon2 - lon1
  diff_lat <- lat2 - lat1
  earth_radius_km <- 6378.137

  a <- sin(diff_lat/2)^2 + cos(lat1) * cos(lat2) * sin(diff_lon/2)^2
  c <- 2 * asin(pmin(1,sqrt(a)))
  return(earth_radius_km * c)
}

#' @keywords internal
euclidean_distance <- function(coord_a, coord_b) {
  stopifnot(identical(dim(coord_a), dim(coord_b)))
  rowSums((coord_a - coord_b)^2)^(1/ncol(x))
}

#' @keywords internal
degree2radian <- function(deg) {
  return(deg*pi/180)
}

#' @keywords internal
radian2degree <- function(rad) {
  return(rad*180/pi)
}
