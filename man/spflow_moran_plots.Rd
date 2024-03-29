% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_generics_and_maybes.R,
%   R/class_spflow_network_multi.R, R/class_spflow_model.R
\name{spflow_moran_plots}
\alias{spflow_moran_plots}
\alias{spflow_moran_plots,spflow_network_multi-method}
\alias{spflow_moran_plots,spflow_model-method}
\title{Moran scatter plots of interaction data}
\usage{
spflow_moran_plots(object, ...)

\S4method{spflow_moran_plots}{spflow_network_multi}(
  object,
  id_net_pair = id(object)[["pairs"]][[1]],
  flow_var,
  model = "model_9",
  DW,
  OW,
  add_lines = TRUE
)

\S4method{spflow_moran_plots}{spflow_model}(object, model = "model_9", DW, OW, add_lines = TRUE)
}
\arguments{
\item{object}{A \code{\link[=spflow_network_multi-class]{spflow_network_multi-class()}} or a \code{\link[=spflow_model-class]{spflow_model-class()}}}

\item{...}{arguments passed to methods}

\item{id_net_pair}{A character indicating the id of a \code{\link[=spflow_network_pair]{spflow_network_pair()}} (only relevant if
the \code{\link[=spflow_network_multi]{spflow_network_multi()}} contains multiple \code{spflow_network_pair}-objects:
defaults to the of them)}

\item{flow_var}{A character, indicating one variable from the network pair data}

\item{model}{A character indicating the model number, that controls different spatial
dependence structures should be one of \code{paste0("model_", 1:9)}.
Details are given in the documentation of \code{\link[=spflow_control]{spflow_control()}}.}

\item{DW, OW}{A matrix to replace the neighborhood of the destinations (DW) and origins (OW).
Defaults to the one supplied to the model.}

\item{add_lines}{A logical, if \code{TRUE} regression lines are added to the Moran scatter plots.}
}
\description{
Generate up to three Moran scatter plots, related to origin-, destination-,
and origin-to-destination-dependence.
}
\examples{

 # Used with a spflow_network_multi ...
 # To check the if there is spatial correlation in any variable
 spflow_moran_plots(multi_net_usa_ge, "ge_ge",flow_var = "y9")


 # Used with a spflow_model...
 # Check the if there is spatial correlation in the residuals
 gravity_ge <- spflow(
   y9 ~ . + P_(DISTANCE),
   multi_net_usa_ge,
   "ge_ge",
   spflow_control(model = "model_1"))

 spflow_moran_plots(gravity_ge)

}
