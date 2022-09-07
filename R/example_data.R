#' @title Simulated data for stylized versions of Germany and the USA
#'
#' @description
#' The package provides two stylized country examples that have been already
#' used by\insertCite{Yang2017;textual}{spflow}.
#' The first example is a stylized version of Germany with 16 states and
#' second one is based on the USA with 51 states.
#'
#'
#' There are two \pkg{sf} objects that contain the geography and data for the country
#' examples.
#' The same information is also provided by two [spflow_network_nodes-class()] objects.
#' Additionally, there is an [spflow_network_multi-class()] that contains the
#' two [spflow_network_nodes()] and four [spflow_network_pairs()], which contain
#' OD-pairs where each states of each country can be origins or destinations.
#'
#' For each of the OD-pairs we simulated three flow vectors `y1`, `y2` and `y9`.
#' The parameters used for this simulation are given as a list:
#' `rho` contains the auto-regression parameters, `delta` contains the parameters
#' for the exogenous variables and `sd_error` is the standard deviation of the Gaussian error term.
#'
#' @author Lukas Dargel
#' @source
#'   The stylized versions of Germany and the USA are inspired by
#'   https://ialab.it.monash.edu/~dwyer/papers/maptrix.pdf
#' @rdname example_data
#' @name example_data
#' @references \insertAllCited{}
"multi_net_usa_ge"

#' @rdname example_data
"germany_grid"

#' @rdname example_data
"usa_net"

#' @rdname example_data
"usa_grid"

#' @rdname example_data
"germany_net"

#' @rdname example_data
"simulation_params"

#' Example data for commuting flows within Paris
#'
#' The package includes data set that contains home-to-work
#' commuting flows for 71 municipalities around the center of Paris.
#' The data for the example is stored in three objects
#'
#' 1. `paris10km_municipalities` contains information on the municipalities.
#' It contains some socio-economic variables, the identifier of the
#' municipality and a geometry column.
#' The geometry is a MULTIPOLYGON that describes the shape of the region.
#'
#' 2. `paris10km_commuteflows` contains information on pairs of municipalities.
#' It is a data.frame with origin and destination identifiers and contains
#' the information on the size of the commuting flows and the distance.
#'
#' 3. `paris10km_neighborhood` contains three sparse matrices that represent
#' alternative definitions of the neighborhood of the municipalities.
#' The first is based on contiguity, the second one on distance and the third
#' one is based on the three nearest neighbors.
#'
#'
#' @source
#' The data combines different public sources (last accessed 2021-05-05).
#'
#' Three data sets are provided by INSEE.
#' More information on the rights to use and diffuse this data is provided \href{https://www.insee.fr/fr/information/2008466}{here}.
#'   * Commuting flows (https://www.insee.fr/fr/statistiques/fichier/3566477/base-texte-flux-mobilite-domicile-lieu-travail-2015.zip)
#'   * Number of companies (https://www.insee.fr/fr/statistiques/2021271)
#'   * Median income (https://www.insee.fr/fr/statistiques/3560121)
#'
#' The geographies, population, and area of the municipalities come from \href{https://public.opendatasoft.com/explore/?sort=modified}{OpenDataSoft} and are available \href{https://public.opendatasoft.com/explore/dataset/code-postal-code-insee-2015/export/?flg=fr&location=2,18.52839,-2.98471&basemap=jawg.streets}{here}.
#' This data  set is published under an \href{https://www.etalab.gouv.fr/wp-content/uploads/2014/05/Licence_Ouverte.pdf}{OPEN LICENCE}.
#'
#' @author Lukas Dargel
#' @rdname paris_data
#' @name paris_data
"paris10km_municipalities"

#' @rdname paris_data
"paris10km_commuteflows"

#' @rdname paris_data
"paris10km_neighborhood"

