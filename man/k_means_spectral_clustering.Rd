% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectral-clustering.R
\name{k_means_spectral_clustering}
\alias{k_means_spectral_clustering}
\title{k_means_spectral_clustering}
\usage{
k_means_spectral_clustering(data, num_cluster, dim = 2, h = 20)
}
\arguments{
\item{data}{matrix. Every row contains a point}

\item{num_cluster}{int. number of clusters desired}

\item{dim}{(optional) int. desired dimension of computed projections. Default: 2}

\item{h}{(optional) int. descent parameter of Gauss kernel}
}
\value{
list containing logical indicating whether the algorithm converged, number of iterations, cluster means and labels
}
\description{
applies k-means algorithm on spectral projections computed by spectral-clustering
}
