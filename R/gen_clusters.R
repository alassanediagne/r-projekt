#' gen_clusters
#'
#' @param n Anzahl an Punkten pro Cluster
#' @param means Mittelpunkte der Cluster
#' @param deviation Standardabweichung bei Lärm
#'
#' @return data
#' @export
#'

gen_clusters <- function(n, means, deviation){

  data <- NULL
  for(i in 1:nrow(means)){
    x <- matrix(rep(means[i,],each=n), ncol=ncol(means), nrow=n) +
      matrix(rnorm(n*ncol(means), sd=deviation), nrow=n)
    data <- rbind(data, x)
  }
  return(data)

}
