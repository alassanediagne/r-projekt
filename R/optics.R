optics <- function(data, eps, minPts) { 
  
  # Initialize reachability distances to infinity
  reachability <- rep(Inf, ncol(data))
  
  # Track processed points
  processed <- rep(FALSE, ncol(data))
  
  # List to maintain the order of points
  ordered_list <- integer(0)
  
  # Calculate the core distance for a given point
  core_distance <- function(point) {
    distances <- sqrt(colSums((data - data[, point])^2))
    sorted_distances <- sort(distances)
    if (sorted_distances[minPts] <= eps) {
      return(sorted_distances[minPts])
    } else {
      return(Inf)
    }
  }
  
  # Update the reachability distances of the neighbors of a point
  update <- function(neighbors, point, seeds, reach_dist) {
    core_dist <- core_distance(point)
    for (i in neighbors) {
      if (!processed[i]) {
        new_reach_dist <- max(core_dist, sqrt(sum((data[, point] - data[, i])^2)))
        if (is.infinite(reach_dist[i])) {
          reach_dist[i] <- new_reach_dist
          seeds <- c(seeds, i)
        } else if (new_reach_dist < reach_dist[i]) {
          reach_dist[i] <- new_reach_dist
        }
      }
    }
    return(seeds)
  }
  
  # Main loop to process each point
  for (i in 1:n) {
    if (!processed[i]) {
      neighbors <- which(sqrt(colSums((data - data[, i])^2)) <= eps)
      processed[i] <- TRUE
      ordered_list <- c(ordered_list, i)
      if (length(neighbors) >= minPts) {
        seeds <- update(neighbors, i, integer(0), reachability)
        while (length(seeds) > 0) {
          seeds <- seeds[order(reachability[seeds])]
          current <- seeds[1]
          seeds <- seeds[-1]
          neighbors <- which(sqrt(colSums((data - data[, current])^2)) <= eps)
          processed[current] <- TRUE
          ordered_list <- c(ordered_list, current)
          if (length(neighbors) >= minPts) {
            seeds <- update(neighbors, current, seeds, reachability)
          }
        }
      }
    }
  }
  
  return(list(ordered_list = ordered_list, reachability = reachability))
}

# Test area:
library(ggplot2)
library(reshape2)
library(tidyverse)

set.seed(42)
data <- rbind(
  matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2),
  matrix(rnorm(100, mean = 3, sd = 0.3), ncol = 2),
  matrix(rnorm(100, mean = 6, sd = 0.3), ncol = 2)
)
data <- t(data)

eps <- 0.5
minPts <- 5

optics_result <- optics(data, eps, minPts)

reachability_distances <- optics_result$reachability
ordered_indices <- optics_result$ordered_list


