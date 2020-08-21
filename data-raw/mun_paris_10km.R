# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - create an exmaple dataset for illustrations and examples
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# The example contains commuting flows between all communities that are within
# a 10km radius of the center of Paris.
# = = = = = = = = = = = = = = = = = = =
# Notes:
#
# The data is prepared with functions that are contained in the package
# spflowDataTool written by Gabriel Watkinson.
# - - - - - - - - - - - - - - - - - - -
# Date: August 2020

devtools::load_all()
library("readr")
library("sf")
library("rio")
library("here")
library("readxl")
library("dplyr")
library("stringr")
library("units")
library("spdep")
library("data.table")
import_pkg <- "spflowDataTool"
pkg_version <- "0.0.0.9002"
has_correct_version <- require_version(import_pkg,pkg_version)
if (!has_correct_version) {
  repo <-  "LukeCe/"
  repo_full <- repo %p% import_pkg %p% "@" %p% pkg_version
  remotes::install_github(repo, auth_token = Sys.getenv("GITHUB_PAT"))
}

# ---- load raw data -----
# solve missing files problem
missing_file_url <- "https://www.insee.fr/fr/statistiques/fichier/3560121/"
missing_file_zip <- "filo-revenu-pauvrete-menage-2015.zip"
download.file(url = missing_file_url %p% missing_file_zip,
              destfile = here::here("data-raw",missing_file_zip))
missing_file_name <-
  "filo-revenu-pauvrete-menage-2015/base-cc-filosofi-2015.xls"
unzip(here::here("data-raw",missing_file_zip),
      files = missing_file_name,
      exdir = here::here("data-raw"),junkpaths = TRUE)

home_to_work_flows <- spflowDataTool:::flow_data(load = TRUE, save = FALSE)
municipality_infos <- spflowDataTool:::town_data(load = TRUE, save = FALSE)
# the file are downloaded in `data\raw_data\` if they don't exist
# the data frames are saved in `data\derived_data\` the first time they are created
# they are then loaded by default, use `load = FALSE` to recreate them

# ---- Extract an example -----------------------------------------------------

# Commuting flows for the municipalities arround paris (10km radius)
center_of_paris <- c(lon = 2.349014,lat = 48.864716)
radius <- 10e3
suppressWarnings({
  mun_of_intrest <- spflowDataTool:::distance_to_com(
    point = center_of_paris,
    data_info = municipality_infos,
    distance = radius)
})

length(mun_of_intrest)^2
# extract the relevant origin/destination data
mun_data <- spflowDataTool:::com_information(
  ID = mun_of_intrest, data_info = municipality_infos)


suppressWarnings({
  ## define diffrent neighborhood matrices
  mun_poly_nb <- spflowDataTool:::com_poly_nb(mun_data)
  mun_dist_nb <- spflowDataTool:::com_dist_nb(mun_data, distance = 5)
  mun_knn_nb <- spflowDataTool:::com_closest_nb(mun_data, k = 10)

  ## generate the distance matrix
  mun_distances <- spflowDataTool:::com_distances(data_info = mun_data)
})


# extract the wanted flows and add distance as well as other variables
suppressWarnings({
  mun_pair_data <- spflowDataTool:::extract_flows(
    data_flows = home_to_work_flows,
    data_info = mun_data, # also works with municipality_infos
    origin = mun_of_intrest,
    var_origin = "CODGEO",              # not needed (default values)
    var_dest = "DCLT",                  # not needed (default values)
    var_ID = "COM_ID",                  # ...
    var_expl = c("population",
                 "superficie",
                 "nbr_entreprises",
                 "salaire_median",
                 "nbr_menages",
                 "prix_m2"))
})
nb_of_na <- sum(is.na(mun_pair_data$flux)) # number of rows with missing flow
pc_of_na <- nb_of_na/nrow(mun_pair_data) # percentage of rows with missing flow
print(pc_of_na) # pc_of_na = 3%

# clean up example data objects

# node data
paris10km_nodes <- mun_data %>%
  select("id" = "COM_ID",
         "pop" = "population",
         "median_income" = "salaire_median",
         "area" = "superficie",
         "companies" = "nbr_entreprises") %>%
  mutate("id" = as.factor(id)) %>%
  st_set_geometry(NULL) %>%
  setDT(key = "id")

# node nieghborhoods
paris10km_mat_nb <- list(
  "by_dist" = mun_dist_nb$mat,
  "by_knn" = mun_knn_nb$mat,
  "by_border" = mun_poly_nb$mat)

# node pair data
paris10km_node_pairs <- mun_pair_data %>%
  select("orig_id" = "CODGEO",
         "dest_id" = "DCLT",
         "commute_flow" = "flux",
         "distance" = "distance") %>%
  mutate_at(.vars = c("orig_id","dest_id"),
            .funs = ~factor(.x, levels = levels(paris10km_nodes$id))) %>%
  setDT(key = c("orig_id","dest_id"))

usethis::use_data(paris10km_nodes,
                  paris10km_mat_nb,
                  paris10km_node_pairs,overwrite = TRUE)

