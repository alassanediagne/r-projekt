compute_distances <- function(data) {
  dist(data, method = "euclidean") %>% as.matrix()
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

k_medoids <- function(data, num_cluster, max_iter = 50L, tol = 1e-8) {

  distance_matrix <- compute_distances(data)
  medoids <- sample(1:nrow(data), num_cluster)

  n_iter <- 0L
  converged <- FALSE

  while (n_iter <= max_iter && !converged) {
    C <- update_C(distance_matrix, medoids)
    new_medoids <- update_medoids(data, C, num_cluster)

    if (all(medoids == new_medoids)) {
      converged <- TRUE
    } else {
      medoids <- new_medoids
    }

    n_iter <- n_iter + 1L
  }

  if (converged) {
    message <- sprintf("Method converged after %i iterations", n_iter)
  } else {
    message <- "Maximum number of iterations reached"
  }

  return(list(
    message = message,
    medoids = data[medoids, , drop = FALSE],
    clusters = C
  ))
}




