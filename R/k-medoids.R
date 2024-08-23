update_labels <- function(data, medoids) {
  # updates labels of data
  distances <- as.matrix(dist(data, method = "euclidean"))[, medoids]
  labels <- apply(distances, 1, which.min)
  return(labels)
}


update_medoids <- function(data, num_cluster, labels, medoids) {
  # updates medoids
  new_medoids <- rep(0, times=num_cluster)
  for (i in 1:num_cluster) {
    cluster_idx <- which(labels == i)
    if (length(cluster_idx) > 0) {
      cluster_points <- data[cluster_idx, ]
      distances <- as.matrix(dist(cluster_points))
      distances <- colSums(distances)
      new_medoids[i] <- cluster_idx[which.min(distances)]
    } else {
      # do not change medoid if the cluster is empty
      new_medoids[i] <- medoids[i]
    }
  }
  return(new_medoids)
}

#' @title k_medoids
#' @name k_medoids
#' @author Aysegul Pekozsoy
#' @param data data matrix. Every row contains a point
#' @param num_cluster int. number of clusters desired
#' @param max_iter (optinal) int. sets maximum number of iterations. Default: 100
#'
#' @return list containing logical indicating whether the algorithm converged, number of iterations, cluster medoids
#' @export
#'
#' @examples
#' data <- gen_clusters(50, matrix(c(0,1,2,1,0,1,2,0),ncol=2), 0.2)
#' k_medoids(data,4)


k_medoids <- function(data, num_cluster, max_iter = 100L) {

  if(num_cluster > nrow(data)){
    stop(sprintf("tried to generate %i clusters with %i data points", num_cluster, nrow(data)))
  }

  n <- nrow(data)
  converged <- FALSE
  n_iter <- 0L

  medoids <- sample(1:n, num_cluster)

  for (i in 1:max_iter) {
    labels <- update_labels(data, medoids)
    new_medoids <- update_medoids(data, num_cluster, labels,medoids)
    n_iter <- n_iter+1
    if (all(medoids == new_medoids)) {
      # converged if medoids do not change
      converged <- TRUE
      break
    }

    medoids <- new_medoids
  }

  return(list(converged = converged, n_iter = n_iter, medoids = data[medoids,], labels = labels))
}


#' plot_k_medoids_2d
#'
#' @param data data matrix. Every row contains a point
#' @param num_cluster int. number of clusters desired
#' @param max_iter (optinal) int. sets maximum number of iterations. Default: 100
#'
#' @return performs k-medoids clustering and plots results
#' @export
#'
#' @examples
#' data <- gen_clusters(50, matrix(c(0,1,2,1,0,1,2,0),ncol=2), 0.2)
#' plot_k_medoids_2d(data,3)
plot_k_medoids_2d <- function(data, num_cluster, max_iter=100L){
  km <- k_medoids(data,num_cluster,max_iter = max_iter)
  if(!km$converged){
    warning("k-medoids did not converge")
  }
  data <- tibble::tibble(x= data[,1], y= data[,2])
  medoids <- tibble::tibble(x= km$medoids[,1], y= km$medoids[,2])
  ggplot2::ggplot() +
    ggplot2::geom_point(data = data, ggplot2::aes(x = x, y = y, color = factor(km$labels)), size=1) +
    ggplot2::geom_point(data = medoids, ggplot2::aes(x = x, y = y), color="red", shape="x", size=5) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="none")
}

