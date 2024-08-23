#'
#'This is an implementation of the DBSCAN Algorithm
#'
#'We're using bfs to find the connected parts
#'
#'@param data The data to be clustered.
#'@param eps The distance defining the neighborhood of a point.
#'@param minPts The minimum point count needed to form a dense region.
#'@export
dbscan <- function(data, eps, minPts) {
  stopifnot(
    "minPts has to be bigger than 1"=minPts>=1
  )

  visited <- rep(FALSE, ncol(data))
  clusters <- list()

  dbscan_bfs <- function(nodes) {
    for (node in nodes) {
      # Check if node has already been visited
      if(visited[node] == FALSE) {

        visited[node] <<- TRUE

        # Calculate distances from the current node
        distances_to_current_node <- (data - data[,node])^2 |> apply(2, sum)

        # decide if current node is a core node
        if (sum(distances_to_current_node <= eps) >= minPts) {
          reachable_neighbours <- which(distances_to_current_node <= eps)

          current_cluster <<- c(current_cluster, reachable_neighbours)

          dbscan_bfs(reachable_neighbours)
        }
      }
    }
  }

  while(!all(visited)) {
    # Get index of first unvisited Node
    univisited_node <- seq_along(visited)[visited==FALSE] %>% gdata::first()
    # Get index of first unvisited Node
    i <- seq_along(visited)[visited==FALSE] %>% gdata::first()

    current_cluster <- c()
    dbscan_bfs(i)

    if(!is.null(current_cluster)) {
      current_cluster <- unique(current_cluster)
      clusters <- c(clusters, list(current_cluster))
    }
  }

  clusters
}
