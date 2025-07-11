% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectral-clustering.R
\name{plot_spectral_clustering_twoclusters}
\alias{plot_spectral_clustering_twoclusters}
\title{plot_spectral_clustering_twoclusters}
\usage{
plot_spectral_clustering_twoclusters(data, h = 20)
}
\arguments{
\item{data}{matrix. Every row contains a point}

\item{h}{(optional) int. descent parameter of Gauss kernel}
}
\value{
cluster-assignment and plot
}
\description{
computes onedimensional projections with spectral clustering and assigns two clusters by comparing to mean.
}
