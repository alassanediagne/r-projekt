library(tidyverse)
library(clusterGeneration)

# Generate sample data
x <- genRandomClust(4, sepVal =0.4)
data <- t(x$datList$test_3)
plot(data['x1',], data['x2',])


#'
#'This is an implementation of the DBSCAN Algorithm
#'
#'We're using bfs to find the connected parts
#'
#'@param data The data to be clustered
#'@param eps The distance defining the neighborhood of a point.
#'@param minPts The minimum point count needed to form a dense region.
dbscan <- function(data, eps, minPts) {
  visited <- rep(FALSE, ncol(data))
  clusters <- list()

  dbscan_bfs <- function(nodes) {
    for (node in nodes) {
      # print(str_glue("Visiting node {node}"))
      # Check if node has already been visited
      if(visited[node] == FALSE) {

        # print(str_glue("Marking node {node} as visited"))
        visited[node] <<- TRUE
        # print(str_glue("Marked node {node} as visited"))

        # Calculate distances from the current node
        distances_to_current_node <- (data - data[,node])^2 |> apply(2, sum)

        # decide if current node is a core node
        if (sum(distances_to_current_node <= eps) >= minPts) {
          current_cluster <<- c(current_cluster, node)
          reachable_neighbours <- distances_to_current_node <= eps
          univisited_reachable_neighbours <- reachable_neighbours & !visited

          current_cluster <<- c(current_cluster, which(reachable_neighbours))

          dbscan_bfs(which(reachable_neighbours))
        }
      }
    }
  }

  while(!all(visited)) {
    # Get index of first unvisited Node
    univisited_node <- seq_along(visited)[visited==FALSE] %>% first()
    # Get index of first unvisited Node
    i <- seq_along(visited)[visited==FALSE] %>% first()

    current_cluster <- c()
    dbscan_bfs(i)

    if(!is.null(current_cluster)) {
      current_cluster <- unique(current_cluster)
      clusters <- c(clusters, list(current_cluster))
    }
  }

  clusters
}

clust <- dbscan(data, 10, 15)

color=c("red", "green", "yellow", "blue", "orange", "purple")

for (i in 1:length(clust)) {
  points(data[1,clust[[i]]], data[2, clust[[i]]], col=color[i])
}

