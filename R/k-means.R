#'
#'k-Means-Algorithmus
#'
#'
#'
#'@param data Matrix mit Daten die geclustered werden sollen. Jede Zeile enthält einen Messwert in R^d
#'@param num_cluster int. Anzahl der Cluster
#'@param m0 (optional) Matrix mit Anfangswerten zur Initialisierung des Algorithmus. Falls nicht angegeben werden die Startwerte automatisch gewählt
#'@param save_history (optional) logical. Gibt Cluster-Mittelpunkte und Labels in jeder Iteration zurück. Default: FALSE
#'@param return_labels (optional) logical. Gibt zu jedem Messwert das Clusterlabel zurück. Default: FALSE
#'@param max_iter (optinal) int. Legt maximale Anzahl an Iterationen fest. Default: 50
#'@param tol: (optinal) float. Toleranz zur Festlegung der Konvergenz. Default: 1e-8
#'
#'@return Liste mit Konvergenznachricht, Clustermittelpunkten, sowie, falls erwünscht Labels und Iterationen
#'
#'@importFrom tibble "tibble"
#'@importFrom magrittr "%>%
#'
#'@export
#'@examples data <- matrix(runif(100), ncol = 2); k_means(data, 5)

usethis::use_package_doc(open = rlang::is_interactive())
usethis::use_pipe(export=F)

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



k_means <- function(data, num_cluster, m0 = NULL, save_history = FALSE,
                    return_labels=FALSE, max_iter = 50L, tol = 1e-8){

  # k-Means-Algorithmus
  # data: nxd - Matrix mit Daten (n: Anzahl an Messwerten, d: Dimension),
  # num_cluster: int, Anzahl der Cluster,
  # m0: num_cluster x d - Matrix, Anfangswerte zur Initialisierung des Algorithmus (optional)
  # save_history: logical, gibt Iterationen zurück (optional)
  # return_labels: logical, gibt zu jedem Messwert das Clusterlabel zurück (optional)
  # max_iter: int, maximale Anzahl an Iterationen (optional)
  # tol: float, Toleranz zur Festlegung der Konvergenz (optional)

  if(is.null(m0)){
    m0 <- k_means_pp(data, num_cluster)
  }

  data <- t(data)

  n_iter <- 0L # zählt Iterationen

  m <- t(m0)

  if(save_history){
    history <- list()
  }

  converged <- FALSE

  while(n_iter <= max_iter){
    m_old <- m # speichere m zum vergleichen
    current_arg_mins <- update_C(data,m) # update argmins
    m <- update_m(data,current_arg_mins,num_cluster) # update means

    if(save_history){
      history <- append(history,list(iteration = n_iter, means = t(m), argmins=current_arg_mins))
    }

    if(norm(m-m_old, type='1')<tol){
      # prüfe konvergenz
      converged <- TRUE
      break
    }

    n_iter <- n_iter + 1L
  }

  out <- list()

  if(converged){
    out$msg <- sprintf("Methode konvergiert nach %i Iterationen", n_iter)
  }
  else{
    out$msg <- sprintf("Maximale Anzahl an Iterationen erreicht")
  }

  out$means <- t(m)

  if(save_history){
    out$history <- history
  }

  if(return_labels){
    out$labels <- current_arg_mins
  }

  return(out)
}


k_means_predict <- function(x, means){
  d <- length(x)
  if(d != ncol(means)){
    stop(sprintf("x hat nicht die richtige Dimension (%i vs %i)", ncol(means),d))
  }
  num_cluster <- nrow(means)
  x <- matrix(rep(x, each=num_cluster), ncol=d)
  dists <- abs(x-means)
  dists <- dists %>% apply(1,\(x) sqrt(sum(x^2)))
  pred_label <- which.min(dists)
  return(pred_label)
}



plot_k_means_2d <- function(data, num_cluster){
  clustering <- k_means(data, num_cluster, return_labels = T)
  data <- tibble::tibble(x=data[,1], y= data[,2])
  means <- tibble::tibble(x=clustering$means[,1], y = clustering$means[,2])
  ggplot2::ggplot() +
    ggplot2::geom_point(data = data, aes(x = x, y = y, color = factor(clustering$labels)), size=1) +
    ggplot2::geom_point(data = means, aes(x = x, y = y), color="red", shape="x", size=5) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="none")
}


data <- gen_clusters(100, matrix(c(0,0,1,1,1,0,0,1), ncol=2),0.3)
plot_k_means_2d(data,4)

iris

ggplot() +
  geom_point(data=iris, aes(x=Petal.Length, y = Petal.Width, color=Species)) +
  theme_light()

irisPetals <- iris %>%
  dplyr::select(c(Petal.Length, Petal.Width)) %>%
  data.matrix()

k_means_iris <- k_means(irisPetals, 3, return_labels = TRUE)

plot_k_means_2d(irisPetals, 3)

k_means_predict(c(4,1), k_means_iris$means)
k_means_iris$means
k_means_iris$labels
irisPetals

k_means_iris
