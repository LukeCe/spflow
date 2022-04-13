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


#' @keywords internal
haversine_distance2 <- function(lon1, lat1, lon2, lat2) {
  dg2rad <- pi/180
  diff_lon <- (lon2 - lon1) * dg2rad
  diff_lat <- (lat2 - lat1) * dg2rad
  earth_radius_km <- 6378.137

  a <- sin(diff_lat/2)^2 + cos(lat1) * cos(lat2) * sin(diff_lon/2)^2
  a[a > 1] <- 1
  c <- 2 * asin(sqrt(a))
  return(earth_radius_km * c)
}

#' @title Simple computation of the great circle distance
#' @details Unit is kilometer
#' @export
haversine_distance <- function(lonlat1, lonlat2) {
  stopifnot(all(dim(lonlat1) == dim(lonlat2)), ncol(lonlat1) == 2)

  dg2rad_half <- pi/360
  sin_diff_lonlat <- sin((lonlat1 - lonlat2) * dg2rad_half)^2

  a <- sin_diff_lonlat[,2] + cos(lonlat1[,2]) * cos(lonlat2[,2]) * sin_diff_lonlat[,1]
  a[a > 1] <- 1

  earth_radius_km <- 6378.137
  d <- asin(sqrt(a)) * (2 * earth_radius_km)
  return(d)
}

#' @keywords internal
euclidean_distance <- function(coord_a, coord_b) {
  stopifnot(identical(dim(coord_a), dim(coord_b)))
  rowSums((coord_a - coord_b)^2)^(1/ncol(x))
}


#' @title Simple computation of a distance matrix
#' @export
distance_matrix <- function(
  coord_a,
  coord_b = NULL,
  distance_type = "euclidean") {


  if (distance_type == "euclidean")
    dfun <- euclidean_distance
  if (distance_type == "haversine")
    dfun <- haversine_distance

  internal_distance <- is.null(coord_b)
  if (internal_distance) {
    n <- nrow(coord_a)

    index_o <- unlist(lapply(seq(2, n ), "seq", n))
    index_d <- unlist(lapply(seq(1, n), function(x) rep(x, n - x)))
    dist_vec <- dfun(coord_a[index_d,, drop = FALSE], coord_a[index_o,, drop = FALSE])
    dist_mat <- matrix(0, n, n)
    dist_mat[index_d + (index_o - 1) * n] <- dist_vec
    return(dist_mat + t(dist_mat))
  }


  n_d <- nrow(coord_a)
  n_o <- nrow(coord_b)
  index_o <- rep(seq_len(n_o), each = n_d)
  index_d <- rep(seq_len(n_d), times = n_o)
  dist_mat <- dfun(coord_a[index_d,, drop = FALSE], coord_a[index_o,, drop = FALSE])
  dim(dist_mat) <- c(n_d, n_o)
  return(dist_mat)
}


#' @keywords internal
degree2radian <- function(deg) {
  return(deg*pi/180)
}

#' @keywords internal
radian2degree <- function(rad) {
  return(rad*180/pi)
}
