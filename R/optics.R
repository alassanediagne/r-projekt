#'
#' This is an implementation of the OPTICS Algorithm
#'
#' This is an implementation of the OPTICS (Ordering Points To Identify Clustering Structure) Algorithm
#'
#' @param data The data to be clustered. A matrix, each row being a point in R^d.
#' @param eps The distance defining the neighborhood of a point.
#' @param minPts The minimum point count needed to form a dense region.
#'
#' @return Returns a list of 2 elements \code{list(ordered_list, reachability)}.
#' The \code{ordered_list} being an indices vector, of the ordering obtained by the OPTICS algorithm and
#' the \code{reachability} being a vector of the reachability distances for each point of \code{data} as obtained by the OPTICS algorithm.
#'
#' @examples data <- gen_clusters(50, matrix(c(0,1,2,1,0,1,2,0),ncol=2), 0.3)
#' eps <- 0.5
#' minPts <- 3
#' optics(data, eps, minPts)
#'
#' @section Source:
#' For more information visit [this website](https://de.wikipedia.org/wiki/OPTICS).
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


#'
#'extract_dbscan
#'
#'Extracts clusters from the result of OPTICS algorithm. Clusters are found by 'horizontally cutting' the
#'reachability plot with an eps_prime value.
#'
#'@param optics_result Uses the result of the OPTIC algorithm (see 'optics')
#'@param eps_prime (optional) value to extract clusters, default is \code{eps} of optics_result.
#'
#'@return Returns a list of 2 elements \code{ordered_labels} and \code{labels}
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



#extract_cluster <-function(data, optics_result = optics_r, res) {
#  num_clusters <- length(unique(res))-1
#  cluster <- list()
#  ordered_data <- data[optics_result$ordered_list,]
#  for (i in 1:num_clusters) {
#    cluster <- c(cluster, list(ordered_data[res == i, ]))
#  }
#  return(cluster)
#}

#'
#'plot_reachability
#'
#'Plots the reachability-plot of the OPTICS algithm. A special kind of 'dendogram'
#'
#'@param optics_result Uses the result of the OPTIC algorithm (see 'optics')
#'
#'@return Barplot with height given by \code{reachability} of \code{optics_result} and each
#'bar representing a datapoint, they are orderd by \code{ordered_list} of \code{optics_result}
#'@export

plot_reachability <- function(optics_result) {
  stopifnot(
    "No reachable points"=any(optics_result$reachability != Inf)
  )
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

#'
#'plot_optics_2d
#'
#'Plots 2-dimensional data and colors the clusters obtained by the OPTICS algorithm accordingly.
#'
#'@param data The data to be clustered. A matrix, each row being a point in R^d.
#'@param optics_result Uses the result of the OPTIC algorithm (see 'optics'), make sure it was run on \code{data}
#'@param eps_prime (optional) value to extract clusters, default is \code{eps} of optics_result.
#'
#'@return Plot using \code{ggplot2}
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

