% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/k-medoids.R
\name{plot_k_medoids_2d}
\alias{plot_k_medoids_2d}
\title{plot_k_medoids_2d}
\usage{
plot_k_medoids_2d(data, num_cluster, max_iter = 100L)
}
\arguments{
\item{data}{data matrix. Every row contains a point}

\item{num_cluster}{int. number of clusters desired}

\item{max_iter}{(optinal) int. sets maximum number of iterations. Default: 100}
}
\value{
performs k-medoids clustering and plots results
}
\description{
plot_k_medoids_2d
}
\examples{
data <- gen_clusters(50, matrix(c(0,1,2,1,0,1,2,0),ncol=2), 0.2)
plot_k_medoids_2d(data,3)
}
