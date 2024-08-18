# TODO: One can optimize runtime by using "Spatial Indexing", but how? - keyword: k-d trees?


optics <- function(data, eps, minPts) {
  
  n <- ncol(data)
  
  reachability <- rep(Inf, n) #Initializing the reachability of each point as "Inf" (UNDEFINED)
  
  processed <- rep(FALSE, n) #Keeping track of which points have been processed
  
  ordered_list <- integer(0) #List to maintain the order of points
  
  #First we define a function to calculate the core distance of a given point.
  #It is by definition the 'smallest distance' w.r.t. eps, s.t. the given point is a core point.
  
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

