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
#' @export

# First we implement the OPTICS algorithm, which returns a clustering of given data, w.r.t eps and minPts.
# The return is an 'ordered list' and  a 'reachability list'.
optics <- function(data, eps, minPts) {
  stopifnot(
    "minPts has to be equal or bigger than 1"= minPts>=1
    )

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
  return(list(ordered_list = ordered_list, reachability = reachability))
}


#Problem:


# Second we implement a function 'extract_dbscan' which returns a clustering w.r.t the minPts from above
# and and eps_prime being a value between 0 and our original eps.


get_more_complex_sample_data <- function() {
  cbind(
    c(1,0),
    c(1.1,0),
    c(1.3,0),
    c(1.4,0),
    c(1,0.2),
    c(1.1,0.2),
    c(1.3,0.4),
    c(1.4,-0.2),
    c(2,0),
    c(2.1,0),
    c(2,0.4),
    c(2.1,0.6),
    c(2,0.3),
    c(2.1,0.1),
    c(2.3,0),
    c(-3,2),
    c(1.6, 3)
  )
}


optics_r <- optics(get_more_complex_sample_data(), eps=0.3, minPts=2)


eps <- 0.3

extract_dbscan <- function(optics_result = optics_r, eps = eps, eps_prime = eps) {

  stopifnot(eps_prime <= eps)

  reachability <- optics_result$reachability
  ordered_list <- optics_result$ordered_list

  clusters <- rep(0, length(ordered_list))
  cluster_id <- 0

  for (i in seq_along(ordered_list)) {
    point <- ordered_list[i]
    if (reachability[point] > eps_prime) {
      #new cluster, if reachability distance is larger than eps
      if (i > 1 && clusters[ordered_list[(i-1)]] > 0) {
        cluster_id <- add(cluster_id,1)

      }
    } else {
      #point is assigned to current cluster
      clusters[point] <- cluster_id
    }
  }

  return(clusters)
}


plot_reachability <- function(optics_result = optics_r) {
  ordered_reachability <- optics_result$reachability[optics_result$ordered_list]
  ordered_reachability[ordered_reachability == Inf] <- 1
  n <- length(ordered_reachability)
  barplot(height = ordered_reachability,
          width = (4/n),
          space = 0,
          xlim=c(0,4),
          ylim=c(0,1))
}







