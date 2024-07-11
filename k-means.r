library(tidyverse)


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



k_means <- function(x, num_cluster, m0, save_history = FALSE, max_iter = 20L, tol = 1e-8){
  # x: dxn - Matrix mit Daten, num_cluster: Anzahl der Cluster, m0: Anfangswerte
  n_iter <- 0L # zählt Iterationen
  m <- m0
  if(save_history){
    history <- list()
  }
  
  while(n_iter <= max_iter){
    m_old <- m # speichere m zum vergleichen
    current_arg_mins <- update_C(x,m) # update argmins
    m <- update_m(x,current_arg_mins,num_cluster) # update means
    if(save_history){
      history <- append(history,list(iteration = n_iter, means = m, argmins=current_arg_mins))
    }
    if(norm(m-m_old, type='1')<tol){
      # prüfe konvergenz
      return(list(means=m, msg=sprintf("Methode konvergiert nach %i Iterationen", n_iter)))
    }
    n_iter <- n_iter + 1L
  }
  
  if(save_history){
    return(return(list(means=m, msg="Maximale Anzahl der Iterationen erreicht", history = history)))
  }
  else{
    return(list(means=m, msg="Maximale Anzahl der Iterationen erreicht"))
  }
}



# Set the seed for reproducibility
set.seed(42)

# Generate data for cluster 1
cluster_1 <- matrix(rnorm(100, mean = 5, sd = 1), ncol = 2) + cbind(rep(5, 50), rep(5, 50))

# Generate data for cluster 2
cluster_2 <- matrix(rnorm(100, mean = 0, sd = 1), ncol = 2) + cbind(rep(0, 50), rep(0, 50))

# Generate data for cluster 3
cluster_3 <- matrix(rnorm(100, mean = 5, sd = 1), ncol = 2) + cbind(rep(5, 50), rep(0, 50))

# Combine the clusters to create the dataset
data <- rbind(cluster_1, cluster_2, cluster_3)
data <- t(data)


m0 <- matrix(c(0,0,1,0,3,3),nrow=2)


k_means(data,3,m0)

