library(tidyverse)

dendogram <- function(data, metric="euclidian", type='complete'){
  d <- ncol(data)
  n <- nrow(data)
  dend <- tibble(idx_1 = numeric(), idx_2 = numeric(), height = numeric())
  distances <- as.matrix(dist(data, method=metric))
  distances[upper.tri(distances,diag=TRUE)] <- NA
  labels <- matrix(nrow=n, ncol=n)
  labels[1,] <- 1:n
  
  for(i in 2:n){
    for(label in labels[i-1,]){
      mask <- labels[i-1,] == label
      for(datapoint in data[mask,]){
        distances <- dist()
      }
    }
    closest <- arrayInd(which.min(distances), .dim=dim(distances))
    dend <- dend %>% add_row(idx_1 = closest[[1]], idx_2 = closest[[2]], height = distances[closest])
    distances[closest] <- NA
    labels[i,] <- labels[i-1,]
    labels[i,closest[[2]]] <- labels[[i, closest[[1]]]]
  }
  return(labels)
}

x <- matrix(rnorm(10), ncol = 2)
x
print(dendogram(x))
plot(x)


hierarchial_clustering <- function(data, metric="euclidian", type='complete'){
  
  
  clusters <- data
  
  #berechne Distanzen
  labels <- matrix(nrow=n, ncol=n)
  labels[1,] <- 1:n
  
  for(i in 2:n){
    distances <- as.matrix(dist(clusters, method=metric))
    distances[upper.tri(distances)] <- NA
    closest <- arrayInd(which.min(distances), .dim=dim(distances))
    dendogram <- dendogram %>% 
      add_row(idx_1 = closest[[1]], idx_2 = closest[[2]], height = min(distances))
    distances[closest] <- NA
    labels[]
  }
  
}
as.factor(1:n)

x <- matrix(rnorm(100), nrow = 5)
x
d <- (dist(x))
m <- as.matrix(dist(x))
m[2,][1:5 > 2]
upper.tri(m)
diag(m) <- NA
m
dist(x)[1]
arrayInd(which.min(m), .dim=dim(m))[[1]]
