% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_sp-network-nodes.R
\docType{class}
\name{sp_network_nodes-class}
\alias{sp_network_nodes-class}
\alias{dat,sp_network_nodes-method}
\alias{dat<-,sp_network_nodes-method}
\alias{id,sp_network_nodes-method}
\alias{id<-,sp_network_nodes-method}
\alias{neighborhood,sp_network_nodes-method}
\alias{neighborhood<-,sp_network_nodes-method}
\alias{nnodes,sp_network_nodes-method}
\title{sp_network_nodes Class}
\usage{
\S4method{dat}{sp_network_nodes}(object)

\S4method{dat}{sp_network_nodes}(object) <- value

\S4method{id}{sp_network_nodes}(object)

\S4method{id}{sp_network_nodes}(object) <- value

\S4method{neighborhood}{sp_network_nodes}(object)

\S4method{neighborhood}{sp_network_nodes}(object) <- value

\S4method{nnodes}{sp_network_nodes}(object)
}
\arguments{
\item{object}{A sp_network_nodes-class}

\item{value}{An object to replace the existing id/data/neighborhood}
}
\description{
An S4 class that contains all information on a single network.
In this representation a network is composed of nodes which must be
identified uniquely by and ID.
Each node is described by variables stored in a data.frame.
The node neighborhood matrix describes strength of links between the nodes
of the network.
The class is constructed by the \code{\link[=sp_network_nodes]{sp_network_nodes()}} function.
}
\section{Slots}{

\describe{
\item{\code{network_id}}{A character that serves as an identifier for the network}

\item{\code{nnodes}}{A numeric that indicates the number of nodes in the network}

\item{\code{node_data}}{A data.frame that contains all information describing the nodes}

\item{\code{node_neighborhood}}{A matrix that describes the neighborhood relations of the nodes}
}}

\examples{
## access the data describing the nodes
new_dat <- dat(germany_net)

# access the id of the network
germany_net2 <- germany_net
id(germany_net2)
id(germany_net2) <- "Germany"

# access the neighborhood matrix of the nodes
neighborhood(germany_net)

# access the number of nodes inside the network
nnodes(germany_net)

}
\seealso{
Other spflow network classes: 
\code{\link{sp_multi_network-class}},
\code{\link{sp_network_pair-class}},
\code{\link{spflow_network_classes}}
}
\concept{spflow network classes}