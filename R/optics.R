#'
#' This is an implementation of the OPTICS Algorithm
#'
#' This is an implementation of the OPTICS (Ordering Points To Identify Clustering Structure) Algorithm
#'
#' @param data The data to be clustered.
#' @param eps The distance defining the neighborhood of a point.
#' @param minPts The minimum point count needed to form a dense region.
#'
#' @return Returns a list of 2 elements \code{list(ordered_list, reachability)}.
#' The \code{ordered_list} being an indices vector, of the ordering obtained by the OPTICS algorithm and
#' the \code{reachability} being a vector of the reachability distances for each point of \code{data} as obtained by the OPTICS algorithm.
#'
#' @import magrittr
#' @import tibble
#' @import dplyr
#'
#' @export
optics <- function(data, eps, minPts) {
  stopifnot(
    "minPts has to be equal or bigger than 1"= minPts>=1
  )
  data <- t(data)
  n <- ncol(data)

  reachability <- rep(Inf, n) #Initializing the reachability of each point as "Inf" (UNDEFINED)

  processed <- rep(FALSE, n) #Keeping track of which points have been processed

  ordered_list <- integer(0) #List to maintain the order of points

  #First we define a function to calculate the core distance of a given point.

  core_distance <- function(point) {
    distances <- sqrt(colSums((data - data[,point])^2))
    sorted_distances <- sort(distances)
    if (sorted_distances[minPts] <= eps) {
      return(sorted_distances[minPts]) # This is the 'smallest distance' in question, if the given point is in fact a core point
    } else {
      return(Inf) # This is the case if the point is either a 'border point' or 'NOISE'
    }
  }

  #Function to update the reachability distances of the neighbors of a point

  update <- function(neighbors, point, seeds, reachability) {
    core_dist <- core_distance(point)
    for (i in neighbors) {
      if (!processed[i]) {
        new_reach_dist <- max(core_dist, sqrt(sum((data[,point]-data[,i])^2)))
        if (is.infinite(reachability[i])) {
          reachability[i] <<- new_reach_dist
          seeds <- c(seeds,i)
        }
        else if (new_reach_dist < reachability[i]) {
          reachability[i] <<- new_reach_dist
        }
      }
    }
    return(seeds)
  }

  #Main loop to process each point

  for (i in 1:n) {
    if (!processed[i]) {
      neighbors <- which(sqrt(colSums((data - data[,i])^2)) <= eps)
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
  return(list(ordered_list = ordered_list, reachability = reachability, eps = eps))
}


# Function to extract clusters from the result of the OPTICS algorithm


extract_dbscan <- function(optics_result = optics_r, eps = optics_r$eps) {

  ordered_reachability <- optics_result$reachability[optics_result$ordered_list]
  n <- length(ordered_reachability)
  clusters <- rep(0,n)
  cluster_id <- 0

  for (point in seq_along(ordered_reachability)) {
    if (ordered_reachability[point] > eps) {
      #new cluster, if reachability distance is larger than eps and postprocessing a point, if its NOISE or not
      if (point < n && ordered_reachability[point+1] != Inf) {
        cluster_id <- cluster_id + 1
        clusters[point] <- cluster_id

      } else {
        clusters[point] <- 0 #point is in cluster "0" if noise
      }
    } else {
      #point is assigned to current cluster
      clusters[point] <- cluster_id
    }

  }
  return(list(ordered_clusters = clusters, labels = clusters[sort(optics_result$ordered_list)] ))
}



#extract_cluster <-function(data, optics_result = optics_r, res) {
#  num_clusters <- length(unique(res))-1
#  cluster <- list()
#  ordered_data <- data[optics_result$ordered_list,]
#  for (i in 1:num_clusters) {
#    cluster <- c(cluster, list(ordered_data[res == i, ]))
#  }
#  return(cluster)
#}

plot_reachability <- function(optics_result = optics_r) {
  ordered_reachability <- optics_result$reachability[optics_result$ordered_list]
  max_value <- 2*max(ordered_reachability[!ordered_reachability == Inf])
  ordered_reachability[ordered_reachability == Inf] <- max_value
  n <- length(ordered_reachability)
  barplot(height = ordered_reachability,
          width = (4/n),
          space = 0,
          xlim=c(0,4),
          ylim=c(0,max_value),
          main = "OPTICS: Reachability Plot"
          )
}

### TODO Sorry for removing this but it brakes the package build
### Need adaptation

#' @import ggplot2
plot_optics_2d <- function(data, optics_result = optics_r ){
  clustering <- extract_dbscan(optics_result)
  data <- tibble(x = data[,1], y = data[,2])
  ggplot() +
    geom_point(data = data, aes(x = x, y = y, color = factor(clustering$labels)), size=1) +
    theme_bw() +
    theme(legend.position="none")
}
