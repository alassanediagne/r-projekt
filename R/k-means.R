library(tidyverse)


k_means_pp <- function(data, num_cluster){
  n <- nrow(data)
  d <- ncol(data)
  
  init_vals <- matrix(0, ncol=d, nrow=num_cluster) #hier werden die Anfangswerte gespeichert
  
  random_idx <- sample(1:n, 1) 
  init_vals[1,] <- data[random_idx, ] # erster Anfangswert ist ein zufälliger Punkt
  
  eucl_dist <- function(data, point) {
    # Funktion zur Berechnung des Abstands
    return(sqrt(rowSums((data - point) ^ 2)))
  }
  
  min_distances <- eucl_dist(data, init_vals[1,])
  
  for(i in 2:num_cluster){
    prob <- min_distances^2
    prob <- prob/sum(prob) 
    new_idx <- sample(1:n, 1, prob = prob) 
    # Wahrscheinlichkeit ist gewichtet, proportianal zu quadriertener Distanz
    init_vals[i,] <- data[new_idx,]
    current_distances <- eucl_dist(data, init_vals[i,])
    dist_to_update <- current_distances < min_distances
    min_distances[dist_to_update] <- current_distances[dist_to_update]
  }
  return(init_vals)
}



update_C <- function(x, m){
  # x dxn Matrix mit Daten, m dxK Matrix mit aktuellen means
  
  n <- ncol(x)
  d <- nrow(x)
  num_cluster <- ncol(m)
  
  # initalisiere Variablen
  # Wir setzen die aktuellen argmins und minimalen Distanzen auf das erste mean
  current_arg_mins <- rep(1L, times=n) # hier speichern wir die aktuellen argmins
  current_min_dist <- x - matrix(m[,1], nrow = d, ncol = n)
  current_min_dist <- apply(current_min_dist, 2, \(x) sum(x^2,na.rm = TRUE)) # hier speichern wir die aktuellen minimalen Distanzen
  
  for(k in 2:num_cluster){
    # gehe alle means ab dem zweiten durch
    current_mean <- matrix(m[,k], nrow = d, ncol = n)
    dist_to_mean <- x - current_mean #subrahiere den aktuellen mean
    dist_to_mean <- apply(dist_to_mean, 2, \(x) sum(x^2)) # bestimme alle quadrierten eukl. Normen
    to_update <- dist_to_mean < current_min_dist # bestimme die Indizes zu denen eine kleinere Distanz gefunden wurde
    current_arg_mins[to_update] <- rep(k, times=sum(to_update)) # update argmins
    current_min_dist[to_update] <- dist_to_mean[to_update] # update minimale Distanzen
  }
  return(current_arg_mins)
}



update_m <- function(x,C,num_cluster){
  # x dxn Matrix mit Daten, C: Vektor mit aktuellen argmins, num_cluster: Anzahl der Cluster
  n <- ncol(x)
  d <- nrow(x)
  m <- matrix(nrow=d, ncol=num_cluster)
  
  for(k in 1:num_cluster){
    mask <- C == k # prüfe für welche der x_i ob k aktuelles argmin ist
    mask <- mask %>% rep(each=d) %>% matrix(ncol=n, nrow=d)
    x_relevant <- x[mask] %>% matrix(nrow=d) # maskiere alle x_i deren aktuellen argmin nicht k ist
    m[,k] <- apply(x_relevant, 1, mean,na.rm=TRUE) # update mean
  }
  return(m)
}



k_means <- function(data, num_cluster, m0 = NULL, save_history = FALSE, max_iter = 50L, tol = 1e-8){
  # x: nxd - Matrix mit Daten, num_cluster: Anzahl der Cluster, m0: Anfangswerte
  if(is.null(m0)){
    m0 <- k_means_pp(data, num_cluster)
  }
  data <- t(data)
  n_iter <- 0L # zählt Iterationen
  m <- t(m0)
  if(save_history){
    history <- list()
  }
  
  while(n_iter <= max_iter){
    m_old <- m # speichere m zum vergleichen
    current_arg_mins <- update_C(data,m) # update argmins
    m <- update_m(data,current_arg_mins,num_cluster) # update means
    if(save_history){
      history <- append(history,list(iteration = n_iter, means = t(m), argmins=current_arg_mins))
    }
    if(norm(m-m_old, type='1')<tol){
      # prüfe konvergenz
      return(list(means=t(m), msg=sprintf("Methode konvergiert nach %i Iterationen", n_iter)))
    }
    n_iter <- n_iter + 1L
  }
  
  if(save_history){
    return(return(list(means=t(m), msg="Maximale Anzahl der Iterationen erreicht", history = history)))
  }
  else{
    return(list(means=t(m), msg="Maximale Anzahl der Iterationen erreicht"))
  }
}






# Test

library(clusterGeneration)

plot_2d_clusters <- function(data, means){
  data <- tibble(x=data[,1], y= data[,2])
  means <- tibble(x=means[,1], y= means[,2])
  ggplot() +
    geom_point(data = data, aes(x = x, y = y), size=1) + 
    geom_point(data = means, aes(x = x, y = y), color="red", shape="x", size=5) +
    theme_bw()
}

data <- genRandomClust(3,sepVal = 0.7)  # generiere test cluster
data <- data$datList$test_3
plot_2d_clusters(data,k_means(data,3)$means)
plot_2d_clusters(data,kmeans(data,3)$centers)





y <- genRandomClust(7,sepVal = 0.1)  # generiere test cluster
data2 <- y$datList$test_1
means2 <- k_means(data2, 7)$means #teste
k_means(data2, 7)
plot_2d_clusters(data2,means2)
plot_2d_clusters(data2,kmeans(data2,7)$centers)
