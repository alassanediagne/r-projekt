#'Algorithmus: spektrales Clustering
#'
#'berechnet k-dimensionale Projektionen zu gegebenen Daten
#'
#'@param data Matrix mit Daten die geclustered werden sollen. Jede Zeile enthält einen Messwert in R^d
#'@param num_cluster int. Anzahl der Cluster
#'@param f_kernel (optional) function.Kernfunktion. Default: Gauss-Kern mit Abstieg h
#'@param dim (optional) int. Dimension der erstellten k-dimensionalen Projektionen durch spektrales Clustern. Default: 2
#'
#'@return k-dimensionale Projektionen (selbes Format wie data)
#'@export
gausskernel <- function(x,y){               #Gausskern definieren
    h <- 50
    exp(-h*(norm(x-y, type="2"))^2)
}


spectral_clustering <- function(data, num_cluster, f_kernel = gausskernel, dim = 2){


    n <- nrow(data)
    affinity_matrix <- matrix(0, nrow=n, ncol=n) #Gewichtsmatrix initialisieren

    for (i in 1:n){
        for (j in 1:n)
            {affinity_matrix[i,j]=f_kernel(data[i,], data[j,])} #Gewichte zwischen Daten bestimmen
    }


    diagonal_matrix <- matrix(0,nrow=n,ncol=n) #Diagonalmatrix initialisieren

    for(i in 1:n){
        a <- 0
        for (j in 1:n){
            a <- a+f_kernel(data[i,],data[j,]) #Werte der Diagonalmatrix bestimmen
            diagonal_matrix[i,i]=a
        }
    }

    normalized_diagonal_matrix <- diagonal_matrix
    for(i in 1:n){
        normalized_diagonal_matrix[i,i]=(normalized_diagonal_matrix[i,i])^(-1/2)
    }


    laplace_matrix <- diagonal_matrix-affinity_matrix #Laplacematrix berechnen



    eigen_L <- eigen(normalized_diagonal_matrix%*%laplace_matrix%*%normalized_diagonal_matrix) #Eigenwerte und -vektoren der Laplacematrix bestimmen
    eigenvectors_L <- eigen_L$vectors[,order(eigen_L$values)] #Eigenvektoren von L nach aufsteigender Größe der Eigenwerte sortieren


    spectral_projections <- matrix(0,  nrow=n, ncol=n) #Matrix mit optimalen Projektionen initialisieren

    for (i in 1:n) #Matrix mit optimalen Projektionen erstellen
        {spectral_projections[,i] = (n^(1/2))*normalized_diagonal_matrix%*%eigenvectors_L[,i]}


    k_spectral_projections <- spectral_projections[,2:(dim+1)] #optimale k-dimensionale Projektionen bestimmen

    return(k_spectral_projections)
}

#'k_means_spectral_clustering
#'
#'Wendet k-means-Algorithmus auf die k-dimesionalen Projektionen zu gegebenen Daten an
#'
#'@param data Matrix mit Daten die geclustered werden sollen. Jede Zeile enthält einen Messwert in R^d
#'@param num_cluster int. Anzahl der Cluster
#'@param f_kernel (optional) function. Kernfunktion. Default: Gauss-Kern mit Abstieg h
#'@param dim (optional) int. Dimension der erstellten k-dimensionalen Projektionen durch spektrales Clustern. Default: 2
#'
#'@return Liste mit Konvergenz logical, Anzahl an Iterationen, Clustermittelpunkten, sowie Labels
#'@export

k_means_spectral_clustering <- function(data, num_cluster, f_kernel = gausskernel, dim = 2){

    k_spectral_projections <- spectral_clustering(data, num_cluster, f_kernel = gausskernel, dim = 2)
    k_means_spectral_clustering_result <- k_means(k_spectral_projections, num_cluster, return_labels = TRUE)
    return(k_means_spectral_clustering_result)
}


#'plot_spectral_clustering
#'
#'veranschaulicht Ergebnis des k-means-Algortithmus angewendet auf die Daten, die durch spektrales Clustering erhalten wurden
#'
#'@param data Matrix mit Daten die geclustered werden sollen. Jede Zeile enthält einen Messwert in R^d
#'@param num_cluster int. Anzahl der Cluster
#'@param f_kernel (optional) function. Kernfunktion. Default: Gauss-Kern mit Abstieg h
#'@param dim (optional) int. Dimension der erstellten k-dimensionalen Projektionen durch spektrales Clustern. Default: 2
#'
#'@return Plot der Cluster-Zuteilung
#'@export

plot_spectral_clustering <- function(data, num_cluster, f_kernel = gausskernel, dim = 2){
    clustering <- k_means_spectral_clustering(data, num_cluster, f_kernel, dim)$labels
    plot(data, col=clustering, pch=19)}
