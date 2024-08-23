compute_distances <- function(data) {
  stats::dist(data, method = "euclidean") %>% as.matrix()
}

update_C <- function(distance_matrix, medoids) {
  apply(distance_matrix, 1, function(x) {
    which.min(x[medoids])
  })
}

update_medoids <- function(data, C, num_cluster) {
  medoids <- numeric(num_cluster)
  for (k in 1:num_cluster) {
    cluster_points <- data[C == k, , drop = FALSE]
    if (nrow(cluster_points) > 0) {
      medoid <- apply(cluster_points, 1, function(point) {
        sum(rowSums((cluster_points - point) ^ 2))
      })
      medoids[k] <- which.min(medoid)
    } else {
      # Handle the case where a cluster has no points assigned to it
      medoids[k] <- sample(1:nrow(data), 1)
    }
  }
  return(medoids)
}

#' @title k-medoids clustering
#' @name k_medoids
#' @param data data matrix. Every row contains a point
#' @param num_cluster int. number of clusters desired
#' @param max_iter (optinal) int. sets maximum number of iterations. Default: 50
#' @param tol (optinal) float. tolerance to conclude convergence Default: 1e-8
#'
#' @return list containing logical indicating whether the algorithm converged, number of iterations, cluster medoids
#' @export
#'
#' @examples
#' data <- gen_clusters(50, matrix(c(0,1,2,1,0,1,2,0),ncol=2), 0.2)
#' k_medoids(data,4)

k_medoids <- function(data, num_cluster, max_iter = 100L, tol = 1e-3) {

  distance_matrix <- compute_distances(data)
  medoids <- sample(1:nrow(data), num_cluster)

  n_iter <- 0L
  converged <- FALSE

  while (n_iter <= max_iter && !converged) {
    C <- update_C(distance_matrix, medoids)
    new_medoids <- update_medoids(data, C, num_cluster)

    if (norm(data[medoids,]-data[new_medoids,], type="2")<tol) {
      converged <- TRUE
    } else {
      medoids <- new_medoids
    }

    n_iter <- n_iter + 1L
  }


  return(list(
    converged = converged,
    n_iter = n_iter,
    medoids = data[medoids,],
    labels = C
  ))
}




