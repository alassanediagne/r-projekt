#'
#' This is an implementation of a Hierarchical Clustering Algorithm. This
#' implementation is agglomerative / bottom-up and single linkage and returns a
#' dendogram
#'
#' @export
#'
hierarchial_clustering <- function(data){

  calculate_distance_between_points <- function(x_ind, y_ind) {
    if(length(x_ind) == 1) {
      (data[x_ind,] - data[y_ind,])**2 |> sum() |> sqrt()
    } else {
      (data[x_ind,] - data[y_ind,])**2 |> apply(1, sum) |> sqrt()
    }
  }

  calculate_distance <- function(x, y) {
    ret_list <- rep(0, length(x))

    for(i in seq_along(x)) {
      tree_1 <- x[[i]]
      tree_2 <- y[[i]]

      flatten_tree_1 <- unlist(tree_1)
      flatten_tree_2 <- unlist(tree_2)

      pointwise_distances <- outer(flatten_tree_1, flatten_tree_2, FUN=calculate_distance_between_points)

      # choose min dist
      ret_list[[i]] <- min(pointwise_distances)
    }
    ret_list
  }

  trees <- as.list(1:nrow(data))

  while (length(trees) >= 2) {
    #calc distances
    product <- outer(trees, trees, FUN=calculate_distance)
    diag(product) <- NA

    min <- which.min(product)
    min_pos <- arrayInd(min, dim(product))

    # join
    min_tree_1 <- trees[[min_pos[1]]]
    min_tree_2 <- trees[[min_pos[2]]]

    trees[[min_pos[1]]] <- list(min_tree_1, min_tree_2)
    trees[min_pos[2]] <- NULL
  }

  return(trees)
}


