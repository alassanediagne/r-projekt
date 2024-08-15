library(tidyverse)

hierarchial_clustering <- function(data, metric="euclidian", type='complete'){
  d <- ncol(data)
  n <- nrow(data)
  
  fused <- tibble(idx = numeric(), height = numeric())
  
  distances <- as.matrix(dist(data))
  distances[upper.tri(distances, diag = TRUE)] <- NA
  
  while(!all(is.na(distances))){
    closest <- arrayInd(which.min(distances), dim(distances))
    fused <- fused %>% add_row(idx = closest, height = distances[closest]) 
    distances[closest] <- NA
    distances[rev(closest)] <- NA
  }
  return(fused)
}
