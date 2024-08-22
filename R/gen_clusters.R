#'
#' Diese Funktion generiert Beispiel Cluster
#'
#' @param n Anzahl an Punkten aus denen ein Cluster bestehen soll
#' @param means Mittelwerte der einzelnen Cluster in einer Matrix, dessen Zeilen
#'    jeweils ein Mittelwert angeben.
#'    Durch die Zeilenanzahl wird die Anzahl der Cluster gesteuert.
#' @param deviation Gibt die Standardabweichung der Werte vom Clustermittelpunt
#'    an. Standardtmäßig ist diese 1.
#' @return Gibt eine double Matrix aus, dessen Zeielen die einzelnen
#'    Punkte bilden.
#'
#' @examples
#' # means <- rbind(c(0, 10), c(5, 5))
#' # gen_clusters(40, means)
#' # gen_clusters(30, means, 0.9)
#'
#' @export
#'
gen_clusters <- function(n, means, deviation=1){

  data <- NULL
  for(i in 1:nrow(means)){
    x <- matrix(rep(means[i,],each=n), ncol=ncol(means), nrow=n) +
      matrix(rnorm(n*ncol(means), sd=deviation), nrow=n)
    data <- rbind(data, x)
  }
  return(data)

}
