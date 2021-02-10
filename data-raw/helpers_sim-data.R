create_grid <- function(sp_centroid) {
  # initialization
  # define the grid
  coords <- sp::coordinates(sp_centroid)
  id <- row.names(sp_centroid)
  nx <- length(unique(coords[, 1]))
  ny <- length(unique(coords[, 2]))
  gt <- sp::GridTopology(c(min(coords[, 1]), min(coords[, 2])), c(1, 1), c(nx, ny))
  grd <- sp::SpatialGrid(gt)
  spix <- as(grd, "SpatialPixels")
  spol <- as(spix, "SpatialPolygons")
  o <- over(spol, sp_centroid)[, 1]
  simu_spdf <- spol[!is.na(o), ]
  row.names(simu_spdf) <- id
  simu_spdf <- sp::SpatialPolygonsDataFrame(simu_spdf, sp_centroid@data)
  simu_spdf$NOM <- row.names(sp_centroid)
  return(simu_spdf)
}
