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

#' @title k-medoids clustering
#' @name k_medoids
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




