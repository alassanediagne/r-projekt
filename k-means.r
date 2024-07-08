library(tidyverse)


update_C <- function(x, m){
  # x dxn Matrix mit Daten, m dxK Matrix mit aktuellen means
  
  n <- ncol(x)
  d <- nrow(x)
  K <- ncol(m)
  
  # initalisiere Variablen
  # Wir setzen die aktuellen argmins und minimalen Distanzen auf das erste mean
  current_arg_mins <- rep(1L, times=n) # hier speichern wir die aktuellen argmins
  current_min_dist <- x - matrix(m[,1], nrow = d, ncol = n)
  current_min_dist <- apply(current_min_dist, 2, \(x) sum(x^2)) # hier speichern wir die aktuellen minimalen Distanzen
  
  for(k in 2:K){
    # gehe alle means ab dem zweiten durch
    current_mean <- matrix(m[,k], nrow = d, ncol = n)
    dist_to_mean <- x - current_mean #subrahiere den aktuellen mean
    dist_to_mean <- apply(dist_to_mean, 2, \(x) sum(x^2)) # bestimme alle quadrierten eukl. Normen
    to_update <- dist_to_mean < current_min_dist
    current_arg_mins[to_update] <- rep(k, times=sum(to_update))
    current_min_dist[to_update] <- dist_to_mean[to_update]
  return(current_arg_mins)
  }
}

x <- matrix(1:28,nrow=7)
x
m <- matrix(1:14, nrow = 7)
update_C(x,m)

update_m <- function(K, x, d, C){
  n <- length(x)
  m <- rep(0, times=K)
  for(k in 1:K){
    mask <- C == k
    mask <- mask %>% rep(times=d) %>% matrix(ncol=n, nrow=d)
    
    x_relevant <- x %>% matrix(ncol=n, nrow=d)
    x_relevant <- c(x_relevant[mask])
    
    mk <- vec_sum(x_relevant, d) / sum(mask)
    m[k] <- mk
  }
  return(m)
}


k_means <- function(x,num_cluster, m0 = NULL, max_iter = 100, tol = 1e-5){
  # x: dxn - Matrix mit Daten, k Anzahl der Cluster, m0 Anfangswerte
  prep <- prepare_data(x)
  x <- prep$data
  d <- prep$dimension
  n_iter <- 0L
  m <- m0
  while(n_iter < max_iter){
    m_old <- m
    n_iter <- m_iter + 1L
    C <- update_C(x,m,d)
    m <- update_m(num_cluster, x, d, C)
    if(vec_norm(m_old-m)<tol){
      return(m)
    }
  }
}

x <- list(c(1,2,3),c(3,2,4),c(1,9,8))
prep <- prepare_data(x)
x <- prep$data
d <- prep$dimension
x<-1:3

c(1,2,3,4) < c(3,2,4,1)

x
x[c(T,F,T)] <- rep(2, times=sum(c(T,F,T)))
x
 v <- c(1,2,3,5,6)
 matrix(v, ncol=5,nrow=length(v))
 
m <- list(c(4,5,6),c(1,2,3))
C <- update_C(x,m,3)
update_m(2,x,3,C)
m <- 3:5
n <- 1:3
seq_along(1:5)
sum(c(T,F,T,F))
matrix(1:12, ncol=3)
