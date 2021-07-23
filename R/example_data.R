#' @title Simulated data for stylized versions of Germany and the USA
#'
#' @description
#' The package uses the same stylized country examples as those presented by
#' by \insertCite{Yang2017;textual}{spflow}.
#' The first example is a stylized version of Germany with 16 states.
#' The second example is a stylized version of the USA with 51 states.
#'
#'
#' We provide the spatial objects which contains the geography of our example
#' as well as the prepared [sp_network_nodes()] objects.
#' Finally there is a multi_network object which contains the two networks and
#' four network pairs based on the same examples.
#'
#' The simulation parameters are given as `rho` for the auto-regressive
#' parameter, `delta` for the impact of the exogenous variables and `sd_error`
#' for standard deviation of the simulated Gaussian noise.
#' They are used to simulate two flow vectors `y2` and `y9` for each network
#' pair inside the `multi_net_usa_ge`.
#'
#' @source
#'   Simulated data inspired by
#'   https://ialab.it.monash.edu/~dwyer/papers/maptrix.pdf
#' @rdname example_data
#' @name example_data
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

#' Example data for commuting flows of paris
#'
#' The package includes an example data set that contains home-to-work
#' commuting flows for 71 municipalities around the centrer of Paris.
#' The data for the example is stored in three objects
#'
#' 1. `paris10km_municipalities` contains information on the municipalities.
#' It contains some socio-economic variable, the identifier of the municipality
#' and a geometry column.
#' The geometry is a MULTIPOLYGON that describes the shape of the region.
#'
#' 2. `paris10km_commuteflows` contains information on pairs of municipalities.
#' It is a data.frame with origin and destination identifiers and contains
#' the information on the size of the flow and the distance.
#'
#' 3. `paris10km_neighborhood` contains three sparse matrices that represent
#' alternative definitions of the neighborhood of the municipalities.
#' The first is based on contiguity, the second one on distance and the third
#' one is based on the three nearest neighbours.
#'
#'
#' @source
#' The data combines different public sources (last accessed 2021-05-05).
#'
#' Three data sets are provided by INSEE.
#' More information on the rights to use and diffuse this data is provided \href{https://www.insee.fr/fr/information/2008466}{here}.
#'   * Commute flows (https://www.insee.fr/fr/statistiques/fichier/3566477/base-texte-flux-mobilite-domicile-lieu-travail-2015.zip)
#'   * Number of companies (https://www.insee.fr/fr/statistiques/2021271)
#'   * Median income (https://www.insee.fr/fr/statistiques/3560121)
#'
#' The geographies, population, and area of the municipalities come from \href{https://public.opendatasoft.com/explore/?sort=modified}{OpenDataSoft} and are available \href{https://public.opendatasoft.com/explore/dataset/code-postal-code-insee-2015/export/?flg=fr&location=2,18.52839,-2.98471&basemap=jawg.streets}{here}.
#' This data is published under an \href{https://www.etalab.gouv.fr/wp-content/uploads/2014/05/Licence_Ouverte.pdf}{OPEN LICENCE}.
#'
#' @rdname paris_data
#' @name paris_data
"paris10km_municipalities"

#' @rdname paris_data
"paris10km_commuteflows"

#' @rdname paris_data
"paris10km_neighborhood"

