#'spectral_clustering
#'
#'@description computes k_dimensional projections of given data by spectral-clustering
#'
#'@param data matrix. Every row contains a point
#'@param h (optional) int. descent parameter of Gauss kernel
#'@param dim (optional) int. desired dimension of computed projections. Default: 2
#'
#'@return matrix containing k-dimensional projections (every row contains a point).
#'@export

spectral_clustering <- function(data, dim = 2, h = 20){

    stopifnot("data must not be empty" = is.null(dim(data))==F,
              "h must be positive" = h>0,
              "dim must be positive" = dim>0)

    gausskernel <- function(x,y){               #define Gauss kernel
      exp(-h*(norm(x-y, type="2"))^2)
    }


    n <- nrow(data)
    affinity_matrix <- matrix(0, nrow=n, ncol=n) #initialize affinity matrix

    for (i in 1:n){
        for (j in 1:n)
            {affinity_matrix[i,j]=gausskernel(data[i,], data[j,])} #determine weight/distance between points
    }


    diagonal_matrix <- matrix(0,nrow=n,ncol=n) #initialize diagonalmatirx

    for(i in 1:n){
        a <- 0
        for (j in 1:n){
            a <- a+gausskernel(data[i,],data[j,]) #determine entries of diagonalmatrix
            diagonal_matrix[i,i]=a
        }
    }

    normalized_diagonal_matrix <- diagonal_matrix
    for(i in 1:n){
        normalized_diagonal_matrix[i,i]=(normalized_diagonal_matrix[i,i])^(-1/2) #determine normalized diagonalmatrix
    }


    laplace_matrix <- diagonal_matrix-affinity_matrix #compute Laplacematrix



    eigen_L <- eigen(normalized_diagonal_matrix%*%laplace_matrix%*%normalized_diagonal_matrix) #determine eigenvectors and eigenvalues
    eigenvectors_L <- eigen_L$vectors[,order(eigen_L$values)] #sort eigenvectors of L in ascending order regarding to its eigenvalues


    spectral_projections <- matrix(0,  nrow=n, ncol=n) #initialize matrix that will contain spectral projections

    for (i in 1:n) #compute matrix with spectral projections
        {spectral_projections[,i] = (n^(1/2))*normalized_diagonal_matrix%*%eigenvectors_L[,i]}


    k_spectral_projections <- spectral_projections[,2:(dim+1)] #determine k-dimensional spectral projections

    return(k_spectral_projections)
}

#'plot_spectral_clustering_twoclusters
#'
#'@description computes onedimensional projections with spectral clustering and assigns two clusters by comparing to mean.
#'
#'@param data matrix. Every row contains a point
#'@param h (optional) int. descent parameter of Gauss kernel
#'
#'@return cluster-assignment and plot
#'@export

plot_spectral_clustering_twoclusters <- function(data, h = 20){

  k_spectral_projections <- spectral_clustering(data, dim = 1, h = h) #compute onedimensional spectral projections

  cluster <- 1:nrow(data)
  for (i in 1:nrow(data)){              #assign index of point to one of two clusters by comparing with mean of all projections
    if (k_spectral_projections[i] > mean(k_spectral_projections))
      cluster[i] <- 1
    else
      cluster[i] <- 2
  }

  plot(data, col = cluster, pch = 19)     #visualize clusters
  return(cluster)
}



#'k_means_spectral_clustering
#'
#'@description applies k-means algorithm on spectral projections computed by spectral-clustering
#'
#'@param data matrix. Every row contains a point
#'@param num_cluster int. number of clusters desired
#'@param h (optional) int. descent parameter of Gauss kernel
#'@param dim (optional) int. desired dimension of computed projections. Default: 2
#'
#'@return list containing logical indicating whether the algorithm converged, number of iterations, cluster means and labels
#'@export


k_means_spectral_clustering <- function(data, num_cluster, dim = 2, h = 20){

    k_spectral_projections <- spectral_clustering(data, dim = dim, h = h)         #compute spectral projections
    k_means_spectral_clustering_result <- k_means(k_spectral_projections, num_cluster)  #apply k-means algorithm on spectral projections
    return(k_means_spectral_clustering_result)
}


#'plot_k_means_spectral_clustering
#'
#'@description visualizes results received from applying k-means algorithm on spectral projections computed by spectral-clustering
#'
#'@param data matrix. Every row contains a point
#'@param num_cluster int. number of clusters desired
#'@param h (optional) int. descent parameter of Gauss kernel
#'@param dim (optional) int. desired dimension of computed projections. Default: 2
#'
#'@return plot of cluster assignment
#'@export

plot_k_means_spectral_clustering <- function(data, num_cluster, dim = 2, h = 20){

    clustering <- k_means_spectral_clustering(data, num_cluster, dim = dim, h = h)$labels   #get cluster assignment made by k-means algorithm
    plot(data, col=clustering, pch=19)}
