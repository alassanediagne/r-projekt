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

k_medoids <- function(data, k) {
  # Initialization
  n <- nrow(data)
  medoids <- sample(1:n, k)
  clusters <- rep(0, n)
  prev_medoids <- NULL
  n_iter <- 0
  converged <- FALSE

  # Iterative process
  while (!converged && n_iter < 100) {
    n_iter <- n_iter + 1

    # Assign points to the nearest medoid
    for (i in 1:n) {
      distances <- apply(data[medoids, ], 1, function(medoid) sum((data[i, ] - medoid)^2))
      clusters[i] <- which.min(distances)
    }

    # Update medoids
    new_medoids <- sapply(1:k, function(c) {
      cluster_points <- data[clusters == c, , drop = FALSE]
      if (nrow(cluster_points) == 0) return(medoids[c])  # Avoid empty clusters
      distances <- as.matrix(dist(cluster_points))
      medoid_index <- which.min(rowSums(distances))
      rownames(cluster_points)[medoid_index]
    })

    # Check for convergence
    converged <- all(medoids == new_medoids)
    medoids <- new_medoids

    # Debugging output
    print(paste("Iteration:", n_iter))
    print("Medoids:")
    print(medoids)
    print("Clusters:")
    print(clusters)

    # Prevent infinite loop in case of non-convergence
    if (n_iter > 100) break
  }

  # Return result
  return(list(medoids = medoids, clusters = clusters, converged = converged, n_iter = n_iter))
}
