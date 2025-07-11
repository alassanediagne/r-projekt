% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/k-means.R
\name{k_means}
\alias{k_means}
\title{k_means}
\usage{
k_means(
  data,
  num_cluster,
  m0 = NULL,
  return_labels = TRUE,
  save_history = FALSE,
  max_iter = 50L,
  tol = 1e-08
)
}
\arguments{
\item{data}{matrix. Every row contains a point}

\item{num_cluster}{int. number of clusters desired}

\item{m0}{(optional) matrix with initial values. Will be chosen automatically if not specified}

\item{return_labels}{(optional) logical. returns cluster label of each instance. Default: TRUE}

\item{save_history}{(optional) logical. returns iteration history}

\item{max_iter}{(optional) int. sets maximum number of iterations. Default: 50}

\item{tol}{(optional) float. tolerance to conclude convergence Default: 1e-8}
}
\value{
List containing \itemize{
\item \code{converged} (logical) indicating whether the algorithm converged
\item  \code{n_iter} (integer) number of iterations,
\item  \code{means} (matrix) cluster means
\item  \code{labels} labels of each data point }
}
\description{
This is an implementation of the k-means algorithm with automatic initial values.
}
\examples{
data <- gen_clusters(50, matrix(c(0,1,2,1,0,1,2,0),ncol=2), 0.3)
k_means(data,4)
data <- gen_clusters(100, matrix(c(0,0,1,1,1,0,0,1), ncol=2),0.3)
plot_k_means_2d(data,4)
data <- gen_clusters(100,matrix(1:30, ncol=6),0.2)
k_means(data,5)
}
\references{
\href{https://link.springer.com/book/10.1007/978-3-662-59354-7}{Richter, 2019}
}
\author{
Alassane Diagne
}
