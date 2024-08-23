#'
#' optics
#'
#' This is an implementation of the OPTICS (Ordering Points To Identify Clustering Structure) algorithm
#'
#' @param data The data to be clustered. A matrix, each row being a point in R^d.
#' @param eps The distance defining the neighborhood of a point.
#' @param minPts The minimum point count needed to form a dense region.
#'
#' @return Returns a list of 2 elements \code{list(ordered_list, reachability)}.\cr
#' The \code{ordered_list} being an indices vector, of the ordering obtained by the OPTICS algorithm and
#' the \code{reachability} being a vector of the reachability distances for each point of \code{data} as obtained by the OPTICS algorithm.
#'
#' @examples data <- gen_clusters(50, matrix(c(0,1,2,1,0,1,2,0),ncol=2), 0.3)
#' eps <- 0.5
#' minPts <- 3
#' optics(data, eps, minPts)
#'
#' @section Source:
#' For more information visit \href{https://de.wikipedia.org/wiki/OPTICS}{this website}.
#'
#' @export


# First we implement the OPTICS algorithm, which returns a clustering of given data, w.r.t eps and minPts.
# The return is an 'ordered list' and  a 'reachability list'.

optics <- function(data, eps, minPts) {
  stopifnot(
    "'minPts' has to be equal or bigger than 1."= minPts>=1,
    "'minPts' has to be numeric." = is.numeric(minPts),
    "'minPts' has to be an integer." = minPts == as.integer(minPts),
    "'minPts' can't be larger then nrow('data')" = minPts <= nrow(data),
    "'eps' has to be positive."= eps>=0,
    "'eps' has to be numeric."= is.numeric(eps),
    "'data' has to be a matrix or vector." = is.matrix(data)|is.vector(data),
    "'data' has to be numeric." = is.numeric(data)
  )
  data <- t(data)
  n <- ncol(data)

  reachability <- rep(Inf, n) #Initializing the reachability of each point as "Inf" (UNDEFINED)

  processed <- rep(FALSE, n) #Keeping track of which points have been processed

  ordered_list <- integer(0) #List to maintain the order of points

  #We define a function to calculate the core distance of a given point.

  core_distance <- function(point) {
    distances <- sqrt(colSums((data - data[,point])^2)) #Computes the euclidean distances to all data points
    sorted_distances <- sort(distances) #Pretty self-explanantory
    if (sorted_distances[minPts] <= eps) { #Checks if the point is a core point with its minPts-th neighbor
      return(sorted_distances[minPts]) #Returns the actual core-distance
    } else {
      return(Inf) #This is the case if the point is either a 'border point' or 'NOISE'
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
      neighbors <- which(sqrt(colSums((data - data[,i])^2)) <= eps) #neighborhood of point i using euclidean distance
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


#'
#'extract_dbscan
#'
#'Extract clusters from the result of OPTICS algorithm. Clusters are found by 'horizontally cutting' the
#'reachability plot with an eps_prime value.
#'
#'@param optics_result result of the OPTICS algorithm (see '\code{?optics}')
#'@param eps_prime (optional) value to extract clusters, default is \code{eps} of optics_result.
#'
#'@return Returns a list of 2 elements \code{ordered_labels} and \code{labels}
#'
#'@examples data <- gen_clusters(50, matrix(c(0,1,2,1,0,1,2,0),ncol=2), 0.3)
#' eps <- 0.5
#' minPts <- 3
#' #Obtain an optics result
#' optics_result <- optics(data, eps, minPts)
#' #execute function with default eps_prime
#' extract_dbscan(optics_result)
#'
#'@export



extract_dbscan <- function(optics_result, eps_prime = optics_result$eps) {

  stopifnot(
    "'eps_prime' must be smaller or equal to eps"=eps_prime<=optics_result$eps,
    "'eps_prime' must be positive" = eps_prime>=0
  )

  ordered_reachability <- optics_result$reachability[optics_result$ordered_list]
  n <- length(ordered_reachability)
  clusters <- rep(0,n)
  cluster_id <- 0

  for (point in seq_along(ordered_reachability)) {
    if (ordered_reachability[point] > eps_prime) {
      #New cluster, if reachability distance is larger than eps and postprocessing a point, if its NOISE or not
      if (point < n && ordered_reachability[point+1] <=eps_prime) {
        cluster_id <- cluster_id + 1
        clusters[point] <- cluster_id

      } else {
        clusters[point] <- 0 #Point is in cluster "0", if noise
      }
    } else {
      #Point is assigned to current cluster
      clusters[point] <- cluster_id
    }

  }
  return(list(ordered_labels = clusters,
              labels = clusters[order(optics_result$ordered_list)] ))
}


#'
#'plot_reachability
#'
#'Plots the reachability-plot of the OPTICS algithm. A special kind of 'dendogram'
#'
#'@param optics_result result of the OPTICS algorithm (see '\code{?optics}')
#'@param extract_dbscan (optional) logical. runs 'extract_dbscan'. Default: FALSE
#'@param eps_prime (optional) value to extract clusters when running 'extract_dbscan', default is \code{eps} of optics_result.
#'
#'
#'@return Barplot with height given by \code{reachability} of \code{optics_result} and each
#'bar representing a datapoint, they are orderd by \code{ordered_list} of \code{optics_result}
#'
#'@examples data <- gen_clusters(50, matrix(c(0,1,2,1,0,1,2,0),ncol=2), 0.3)
#' eps <- 0.5
#' minPts <- 3
#' #Obtain an optics result
#' optics_result <- optics(data, eps, minPts)
#' #Plot reachability:
#' plot_reachability(optics_result)
#'
#'@export

plot_reachability <- function(optics_result, extract_dbscan = FALSE, eps_prime = NULL) {
  stopifnot(
    "No reachable points" = any(optics_result$reachability != Inf)
  )

  # extract clustering results if required
  if (extract_dbscan) {
    if (is.null(eps_prime)) {
      db <- extract_dbscan(optics_result)
      cut <- optics_result$eps
    } else {
      db <- extract_dbscan(optics_result, eps_prime)
      cut <- eps_prime
    }
    col <- factor(db$ordered_labels)
  } else {
    col <- rep("grey", length(optics_result$reachability)) # default color if no clustering
    cut <- NULL
  }

  ordered_reachability <- optics_result$reachability[optics_result$ordered_list]
  max_value <- 2 * max(ordered_reachability[!ordered_reachability == Inf])
  ordered_reachability[ordered_reachability == Inf] <- max_value
  n <- length(ordered_reachability)



  barplot(height = ordered_reachability,
          width = (4 / n),
          space = 0,
          xlim = c(0, 4),
          ylim = c(0, max_value),
          xlab = "Ordered Data",
          ylab = "Reachability",
          main = "OPTICS: Reachability Plot",
          col = col,
          border = F
  )

  if (!is.null(cut)) {
    abline(h = cut, col = "red", lty = 2)  # add dotted line at eps value
    text(x = 0.1, y = cut + 0.05 * max_value, labels = paste("eps ≈", cut) , col = "red", pos = 4)
  }
}

#'
#'plot_optics_2d
#'
#'Plots 2-dimensional data and colors the clusters obtained by the OPTICS algorithm accordingly.
#'
#'@param data The data to be clustered. A matrix, each row being a point in R^d.
#'@param optics_result result of the OPTICS algorithm (see '\code{?optics}'), make sure it was run on \code{data}
#'@param eps_prime (optional) value to extract clusters, default is \code{eps} of optics_result.
#'
#'@return Plot using \code{ggplot2}
#'
#'@section Details:
#'This function calls \code{extarct_dbscan}, hence eps_prime can be added as an optional parameter.
#'
#'@export

plot_optics_2d <- function(data, optics_result, eps_prime = optics_result$eps) {

  clustering <- extract_dbscan(optics_result, eps_prime)
  data <- tibble::tibble(x = data[,1], y = data[,2])
  data$label <- clustering$labels
  unique_labels <- unique(data$label[data$label != 0])

  colors <- c("black", scales::hue_pal()(length(unique_labels))) #Generate colors: black for label 0 (noise), and a palette for the rest
  names(colors) <- c(0, unique_labels) #Create a named vector for the color scale

  legend_labels <- c("Noise", paste("Cluster", seq_along(unique_labels)))
  names(legend_labels) <- c(0, unique_labels)

  ggplot2::ggplot() +
    ggplot2::geom_point(data = data, ggplot2::aes(x = x, y = y, color = factor(label)), size = 1) +
    ggplot2::scale_color_manual(values = colors, labels = legend_labels) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "right") +
    ggplot2::labs(color = "Clusters")
}

