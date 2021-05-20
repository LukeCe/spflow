# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - create an example dataset for illustrations
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# The example contains commuting flows between all communities that are within
# a 10km radius of the center of Paris.
# = = = = = = = = = = = = = = = = = = =
# Date: Mai 2021

library("Matrix")
library("readxl")
library("sf")
library("spdep")
library("units")

# ---- load data --------------------------------------------------------------
data_dir <- "data-raw/data_paris10km"
files <- lapply(list(
  "flows" = "base-texte-flux-mobilite-domicile-lieu-travail-2015.txt",
  "mun_info_inc" = "base-cc-filosofi-2015.xls",
  "mun_info_emp" = "base-cc-demo-entreprises-2015.xls",
  "mun_polygons" = "code-postal-code-insee-2015.geojson"),
  function(.f) file.path(data_dir,.f))

flows <- read.delim(files$flows,sep =  ";")
mun_info_inc <- read_xls(files$mun_info_inc, sheet = 1, skip = 5)
mun_info_emp <- rbind(
  read_xls(files$mun_info_emp, sheet = 1, skip = 5), # com
  read_xls(files$mun_info_emp, sheet = 2, skip = 5)) # arr
mun_geo <- read_sf(files$mun_polygons)
mun_geo <- unique(mun_geo[c("insee_com","population","superficie")])

# ---- generate node data -----------------------------------------------------
center_of_paris <- c(lon = 2.349014, lat = 48.864716)
center_of_paris <- st_sfc(st_point(center_of_paris), crs = st_crs(mun_geo))
radius <- set_units(10000, "m")

suppressWarnings({
  dist_to_center <- st_distance(
    st_geometry(st_centroid(mun_geo)),
    center_of_paris)
})

# combine the municipalities data and adjust the names
mun_10km <- mun_geo$insee_com[dist_to_center < radius]
pair10km_info <- mun_info_inc[mun_info_inc$CODGEO %in% mun_10km,]
pair10km_info <- merge(pair10km_info,mun_info_emp,by = "CODGEO")
pair10km_info <- merge(pair10km_info,mun_geo,
                       by.x = "CODGEO", by.y = "insee_com")


# c("NEW_NAME" = "old_name")
keep_cols <-c(
  "ID_MUN"     = "CODGEO",
  "POPULATION" = "population",
  "MED_INCOME" = "MED15",
  "NB_COMPANY" = "ENNTOT15",
  "AREA"       = "superficie",
  "geometry"   = "geometry")
paris10km_municipalities <- st_as_sf(pair10km_info)[,keep_cols]
names(paris10km_municipalities) <- names(keep_cols)

# ---- generate pair data -----------------------------------------------------
suppressWarnings({
  pair_distance <- as.vector(st_distance(st_centroid(paris10km_municipalities)))
})
n <- nrow(paris10km_municipalities)
paris10km_commuteflows <- data.frame(
  "ID_ORIG" = rep(paris10km_municipalities$ID_MUN, n),
  "ID_DEST" = rep(paris10km_municipalities$ID_MUN, each = n),
  "DISTANCE" = pair_distance)

# c("NEW_NAME" = "old_name")
keep_cols <- c("ID_ORIG" = "CODGEO", "ID_DEST" = "DCLT",
               "COMMUTE_FLOW" = "NBFLUX_C15_ACTOCC15P")
paris10km_flows <- flows[
  flows$CODGEO %in% mun_10km & flows$DCLT %in% mun_10km,
  keep_cols]
names(paris10km_flows) <- names(keep_cols)

paris10km_commuteflows <- merge(
  paris10km_commuteflows, paris10km_flows,
  by = c("ID_ORIG", "ID_DEST"),
  all.x = TRUE)
paris10km_commuteflows[
  is.na(paris10km_commuteflows$COMMUTE_FLOW),
  "COMMUTE_FLOW"] <- 0

# ---- generate neighborhoods -------------------------------------------------
suppressWarnings({
  mid_points <- paris10km_municipalities %>%
    st_geometry() %>%
    st_point_on_surface()

  paris10km_neighborhood <- lapply(list(
    "by_contiguity" = poly2nb(paris10km_municipalities),
    "by_distance" = dnearneigh(mid_points,d1 = 0, d2 = 5),
    "by_knn" = knn2nb(knearneigh(mid_points,3))),
    function(.nb) Matrix(nb2mat(.nb)))
})

# ---- export -----------------------------------------------------------------
save(paris10km_neighborhood, file =  "data/paris10km_neighborhood.rda")
save(paris10km_municipalities, file =  "data/paris10km_municipalities.rda")
save(paris10km_commuteflows, file =  "data/paris10km_commuteflows.rda")

