library(tidyverse)


hierarchial_clustering <- function(data, metric="euclidian", type='complete'){
  n <- ncol(data)
  d <- nrow(data)
  
  #berechne Distanzen
  distances <- dist(x, method=metric)
  labels <- matrix(nrow=n, ncol=n)
  labels[1,] <- 1:n
  
  for(i in 2:n){
    
  }
  
}

x <- matrix(rnorm(100), nrow = 5)
x
dist(x)
m <- as.matrix(dist(x))
m
dist(x)[1]
