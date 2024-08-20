
gausskernel <- function(x,y) {exp(-h*(norm(x-y, type="2"))^2)} #Gausskern definieren
h <- 50

spectral_clustering <- function(data, f_kernel, k,clusters){
    
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
    
    
    k_spectral_projections <- spectral_projections[,2:(k+1)] #optimale k-dimensionale Projektionen bestimmen
    
    spectral_clustering_result <- kmeans(k_spectral_projections, centers = clusters)$cluster
    plot(data, col=spectral_clustering_result, pch=19)
}
