#erster Draft

gausskernel <- function(x,y) {exp(-h*(norm(x-y, type="2"))^2)}
h <- 10

spectral_clustering <- function(data, f_kernel, k){
    
    n <- ncol(data) 
    W <- matrix(0, nrow=n, ncol=n) #Gewichtsmatrix initialisieren
   
    i <- 1
    j <- 1
    for (i in 1:n){
        for (j in 1:n)
            {W[i,j]=f_kernel(data[,i], data[,j])} #Gewichte zwischen Daten bestimmen
    }
    
    D <- matrix(0,nrow=n,ncol=n) #Diagonalmatrix initialisieren
    i <- 1
    j <- 1
    for(i in 1:n){
        a <- 0
        for (j in 1:n){
            a <- a+f_kernel(data[,i],data[,j]) #Werte der Diagonalmatrix bestimmen
            D[i,i]=a
        }
    }
    
    L <- D-W #Laplacematrix berechnen
    
    "%^%" <- function(x, q) #Funktion zur Berechnung von negativen, nicht ganzen Potenzen von Matrizen definieren
        {with(eigen(x), vectors %*% (values^q * t(vectors)))}
   
    
    eigen_L <- eigen((D%^%(-0.5))*L*(D%^%(-0.5))) #Eigenwerte und -vektoren der Laplacematrix bestimmen
    eigenvectors_L <- eigen_L$vectors[,order(eigen_L$values)] #Eigenvektoren von L nach aufsteigender Größe der Eigenwerte sortieren
    eigenvectors_logical_L <- logical(n) #Wahrheitsvektor für genormte Eigenvektoren initialisieren
    
    for (i in 1:n){
        if (norm(eigenvectors_L[,i], type="2")==1) #Eigenvektoren mit Norm=1 bestimmen
            eigenlog[i]=TRUE
        }
    eigenvectors_norm_L <- e$vectors[,eigenlog] #Matrix mit Eigenvektoren der Länge 1 als Spalten erstellen       
    
    spectral_projections <- matrix(0,nrow=n, ncol=n) #Matrix mit optimalen Projektionen initialisieren
    
    i <- 1
    for (i in 1:length(eigenlog)) #Matrix mit optimalen Projektionen erstellen
        {spectral_projections[i,] = (n^(1/2))*(D%^%(-0.5))%*%eigenvectors_norm_L[,i]}        
    
    
    k_spectral_projections <- spectral_projections[2:(k+1),] #optimale k-dimensionale Projektionen bestimmen
    return(k_spectral_projections)
}

#anschließend z.B. k-means clustering