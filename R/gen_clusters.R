#'gen_cluster
#'@description this function creates pseudo random data points that can be used for clustering
#'
#' @param n number of points in each cluster
#' @param means matrix. means of clusters
#'    Durch die Zeilenanzahl wird die Anzahl der Cluster gesteuert.
#' @param deviation standard deviation. Default: 0.5
#' @return returns double matrix with data
#'
#' @examples
#' # means <- rbind(c(0, 10), c(5, 5))
#' # gen_clusters(40, means)
#' # gen_clusters(30, means, 0.9)
#'
#' @export
#'
gen_clusters <- function(n, means, deviation=0.5){

  data <- NULL
  for(i in 1:nrow(means)){
    x <- matrix(rep(means[i,],each=n), ncol=ncol(means), nrow=n) +
      matrix(rnorm(n*ncol(means), sd=deviation), nrow=n)
    data <- rbind(data, x)
  }
  return(data)

}
