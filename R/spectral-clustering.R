#erster Draft

gausskernel <- function(x,y) {exp(-h*(norm(x-y, type="2"))^2)} #Gausskern definieren
h <- 100

spectral_clustering <- function(data, f_kernel, k){
    
    n <- ncol(data) 
    affinity_matrix <- matrix(0, nrow=n, ncol=n) #Gewichtsmatrix initialisieren
   
    for (i in 1:n){
        for (j in 1:n)
            {affinity_matrix[i,j]=f_kernel(data[,i], data[,j])} #Gewichte zwischen Daten bestimmen
    }
    
    
    diagonal_matrix <- matrix(0,nrow=n,ncol=n) #Diagonalmatrix initialisieren
   
    for(i in 1:n){
        a <- 0
        for (j in 1:n){
            a <- a+f_kernel(data[,i],data[,j]) #Werte der Diagonalmatrix bestimmen
            diagonal_matrix[i,i]=a
        }
    }
    
    laplace_matrix <- diagonal_matrix-affinity_matrix #Laplacematrix berechnen
    
    "%^%" <- function(x, q) #Funktion zur Berechnung von negativen, nicht ganzen Potenzen von Matrizen definieren
        {with(eigen(x), vectors %*% (values^q * t(vectors)))}
   
    
    eigen_L <- eigen((diagonal_matrix%^%(-0.5))%*%laplace_matrix%*%(diagonal_matrix%^%(-0.5))) #Eigenwerte und -vektoren der Laplacematrix bestimmen
    eigenvectors_L <- eigen_L$vectors[,order(eigen_L$values)] #Eigenvektoren von L nach aufsteigender Größe der Eigenwerte sortieren
    eigenvectors_logical_L <- logical(n) #Wahrheitsvektor für genormte Eigenvektoren initialisieren
    
    for (i in 1:n){
        if (norm(eigenvectors_L[,i], type="2")==1) #Eigenvektoren mit Norm=1 bestimmen
            eigenvectors_logical_L[i]=TRUE
        }
    eigenvectors_norm_L <- eigen_L$vectors[,eigenvectors_logical_L] #Matrix mit Eigenvektoren der Länge 1 als Spalten erstellen       
    
    spectral_projections <- matrix(0,nrow=ncol(eigenvectors_norm_L), ncol=n) #Matrix mit optimalen Projektionen initialisieren
    
    for (i in 1:ncol(eigenvectors_norm_L)) #Matrix mit optimalen Projektionen erstellen
        {spectral_projections[i,] = (n^(1/2))*(diagonal_matrix%^%(-0.5))%*%eigenvectors_norm_L[,i]}        
    
    
    k_spectral_projections <- spectral_projections[2:(k+1),] #optimale k-dimensionale Projektionen bestimmen
    return(k_spectral_projections)
}

#anschließend z.B. k-means clusterings