library(tidyverse)
library(R6)

# Function to compute distance matrix
compute_distances <- function(data) {
  dist(data, method = "euclidean") %>% as.matrix()
}

# Function to update cluster assignments
update_C <- function(distance_matrix, medoids) {
  apply(distance_matrix, 1, function(x) {
    which.min(x[medoids])
  })
}

# Function to update medoids
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

# k-Medoids function
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

# Plotting function for 2D clusters
plot_2d_clusters <- function(data, num_cluster) {
  clustering <- k_medoids(data, num_cluster)
  data <- tibble(x = data[, 1], y = data[, 2])
  medoids <- tibble(x = clustering$medoids[, 1], y = clustering$medoids[, 2])
  ggplot() +
    geom_point(data = data, aes(x = x, y = y, color = factor(clustering$clusters)), size = 1) +
    geom_point(data = medoids, aes(x = x, y = y), color = "red", shape = "x", size = 5) +
    theme_bw() +
    theme(legend.position = "none")
}

# Test

library(clusterGeneration)

# Generate test data
data <- genRandomClust(9, sepVal = 0.12)
data <- data$datList$test_3

# Test k-medoids clustering
result <- k_medoids(data, 9)

# Print the results
print(result$message)
print(result$medoids)
print(result$clusters)

# Plot the clusters
plot_2d_clusters(data, 9)




