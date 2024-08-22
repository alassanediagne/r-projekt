k_means_pp <- function(data, num_cluster){
  n <- nrow(data)
  d <- ncol(data)

  init_vals <- matrix(0, ncol=d, nrow=num_cluster) # matrix for initial values

  random_idx <- sample(1:n, 1)
  init_vals[1,] <- data[random_idx, ] # first selection is a random point

  eucl_dist <- function(data, point) {
    # auxiliary function to compute distance
    return(sqrt(rowSums((data - point) ^ 2)))
  }

  min_distances <- eucl_dist(data, init_vals[1,])

  for(i in 2:num_cluster){
    prob <- min_distances^2
    prob <- prob/sum(prob)
    new_idx <- sample(1:n, 1, prob = prob)
    # probability is proporitional to d^2
    init_vals[i,] <- data[new_idx,]
    current_distances <- eucl_dist(data, init_vals[i,])
    dist_to_update <- current_distances < min_distances
    min_distances[dist_to_update] <- current_distances[dist_to_update]
  }
  return(init_vals)
}


update_arg_mins <- function(x, m){
  # initialize variables
  n <- ncol(x)
  d <- nrow(x)
  num_cluster <- ncol(m)


  # set current labels to 1 and min distances to first mean
  current_arg_mins <- rep(1L, times=n) # vector to save current arg mins
  current_min_dist <- x - matrix(m[,1], nrow = d, ncol = n)
  current_min_dist <- apply(current_min_dist, 2, \(x) sum(x^2,na.rm = TRUE)) # vector saving current min distances

  for(k in 2:num_cluster){
    # itearte through all means starting at the second
    current_mean <- matrix(m[,k], nrow = d, ncol = n)
    dist_to_mean <- x - current_mean
    dist_to_mean <- apply(dist_to_mean, 2, \(x) sum(x^2)) # compute all squared eucl. distances
    to_update <- dist_to_mean < current_min_dist # compute indices for which we have found a closer cluster
    current_arg_mins[to_update] <- rep(k, times=sum(to_update)) # update argmins
    current_min_dist[to_update] <- dist_to_mean[to_update] # update min distances
  }
  return(current_arg_mins)
}



update_means <- function(x,C,num_cluster){

  n <- ncol(x)
  d <- nrow(x)
  m <- matrix(nrow=d, ncol=num_cluster)

  for(k in 1:num_cluster){
    mask <- C == k # check which points are in the kth cluster
    if(sum(mask)==0){
      # if the cluster is empty, choose a random point
      random_idx <- sample(1:n, 1)
      m[,k] <- x[,random_idx]
    }
    else{
      mask <- mask |> rep(each=d) |> matrix(ncol=n, nrow=d)
      x_relevant <- x[mask] |> matrix(nrow=d) # maskiere alle x_i deren aktuellen argmin nicht k ist
      m[,k] <- apply(x_relevant, 1, mean,na.rm=TRUE) # update mean
    }
  }
  return(m)
}

#'@name k_means
#'@title k-means algorithm
#'@description Implementation of the k-means algorithm with automatic initial values (Richter, algorithm 9.10)
#'@param data matrix. Every row contains a point
#'@param num_cluster int. number of clusters desired
#'@param m0 (optional) matrix with initial values. Will be chosen automatically if not specified
#'@param return_labels (optional) logical. returns cluster label of each instance. Default: TRUE
#'@param save_history (optional) logical. returns iteration history
#'@param max_iter (optinal) int. sets maximum number of iterations. Default: 50
#'@param tol (optinal) float. tolerance to conclude convergence Default: 1e-8
#'@return list containing logical indicating wether the algorithm converged, number of iterations, cluster means and labels
#'@examples
#' data <- gen_clusters(50, matrix(c(0,1,2,1,0,1,2,0),ncol=2), 0.3)
#' k_means(data,4)
#'@examples
#' data <- gen_clusters(100, matrix(c(0,0,1,1,1,0,0,1), ncol=2),0.3)
#' plot_k_means_2d(data,4)
#'
#'@export

k_means <- function(data, num_cluster, m0 = NULL, return_labels=TRUE,
                    save_history = FALSE, max_iter = 50L, tol = 1e-8){


  if(is.null(m0)){
    m0 <- k_means_pp(data, num_cluster) # k-means ++ if m0 not specified
  }

  if(num_cluster > nrow(data)){
    stop(sprintf("tried to generate %i clusters with %i data points", num_cluster, nrow(data)))
  }

  if(ncol(m0) != ncol(data)){
    stop(sprintf("data has dimension %i but initial values have dimension %i", ncol(m0), ncol(data)))
  }

  if(nrow(m0) != num_cluster){
    stop(sprintf("initial values do not match number of clusters %i vs. %i", nrow(m0), num_cluster))
  }


  data <- t(data)


  n_iter <- 0L # counter

  m <- t(m0)

  if(save_history){
    history <- list()
  }

  converged <- FALSE

  while(n_iter <= max_iter){
    m_old <- m # save m to compare
    current_arg_mins <- update_arg_mins(data,m) # update argmins
    m <- update_means(data,current_arg_mins,num_cluster) # update means

    if(save_history){
      history <- append(history,list(iteration = n_iter, means = t(m), argmins=current_arg_mins))
    }

    if(norm(m - m_old, type="i") < tol) {
      # check convergence
      converged <- TRUE
      break
    }

    n_iter <- n_iter + 1L
  }

  out <- list()

  out$converged <- converged

  out$niter <- n_iter

  out$means <- t(m)

  if(save_history){
    out$history <- history
  }

  if(return_labels){
    out$labels <- current_arg_mins
  }

  return(out)
}




#' k_means_predict
#'
#' @param x atomic vector or matrix with measurements
#' @param means cluster means (e.g. from k-means algorithm)
#'
#' @return predicted labels of new measurements
#' @export
#'
#' @examples data <- gen_clusters(50, matrix(c(0,1,2,0,1,2), ncol=2), 0.3)
#'  clustering <- k_means(data,3)
#'  k_means_predict(c(1.2,0.8), clustering$means)
#'
k_means_predict <- function(x, means){
  predict_instance <- function(x, means){
    d <- length(x)
    if(d != ncol(means)){
      stop(sprintf("x does not have the right dimension (%i vs %i)", ncol(means),d))
    }
    num_cluster <- nrow(means)
    x <- matrix(rep(x, each=num_cluster), nrow=num_cluster, ncol=d)
    dists <- abs(x-means)
    dists <- dists |> apply(1,\(x) sqrt(sum(x^2)))
    pred_label <- which.min(dists)
    return(pred_label)
  }
  if(is.atomic(x) && !is.matrix(x)){
    return(predict_instance(x,means))
  }
  else if(is.matrix(x)){
    return(apply(x, 1, \(xi) predict_instance(xi,means)))
  }
  else{
    stop(sprintf("x has to be an atomic vector or a matrix but is of class %s", class(x)))
  }
}

#' plot_k_means_2d
#'
#' @param data data that should be clustered and plotted
#' @param num_cluster number of clusters for k-means
#' @param max_iter maximum iterations
#'
#' @return Performs k-means algorithm on data and plots clusters
#' @export
#'
plot_k_means_2d <- function(data, num_cluster, max_iter=50L){
  if(ncol(data) != 2){
    stop("function only plots 2d data")
  }
  clustering <- k_means(data, num_cluster, return_labels = T, max_iter = max_iter)
  data <- tibble::tibble(x=data[,1], y= data[,2])
  means <- tibble::tibble(x=clustering$means[,1], y = clustering$means[,2])
  ggplot2::ggplot() +
    ggplot2::geom_point(data = data, ggplot2::aes(x = x, y = y, color = factor(clustering$labels)), size=1) +
    ggplot2::geom_point(data = means, ggplot2::aes(x = x, y = y), color="red", shape="x", size=5) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="none")
}


