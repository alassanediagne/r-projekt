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



k_means <- function(x, num_cluster, m0, save_history = FALSE, max_iter = 50L, tol = 1e-8){
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



# Test

library(clusterGeneration)

x <- genRandomClust(3)  # generiere test cluster
data <- t(x$datList$test_1)
data

plot(t(data))
k_means(data, 3,m0 = matrix(c(-5,-5,0,0,-5,10),nrow=2)) #teste
kmeans(t(data),3) #vergleiche mit kmeans in R

y <- genRandomClust(7,sepVal = 0.1)  # generiere test cluster
data2 <- t(y$datList$test_1)
means <- k_means(data2, 7,m0 = matrix(1:14,nrow=2))$means #teste
means <- tibble(x=means[1,], y=means[2,])
kmeans(t(data2),7) #vergleiche mit kmeans in R

data2 <- tibble(x=data2[1,], y= data2[2,])

data2 %>% ggplot(mapping = aes(x=x, y= y)) +
  geom_point(size=1) +
  theme_bw()
means %>% ggplot(mapping = aes(x=x, y=y)) +
  geom_point(colour="red")


ggplot() +
  geom_point(data = data2, aes(x = x, y = y), size=1) + 
  geom_point(data = means, aes(x = x, y = y), color="red", shape="x", size=5) +
  theme_bw()


