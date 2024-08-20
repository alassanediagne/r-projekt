get_two_point_data <- function() {
  cbind(c(1, 0), c(2, 0))
}

get_more_complex_sample_data <- function() {
  cbind(
    c(1,0),
    c(1.1,0),
    c(1.3,0),
    c(1.4,0),
    c(1,0.2),
    c(1.1,0.2),
    c(1.3,0.4),
    c(1.4,-0.2),
    c(2,0),
    c(2.1,0),
    c(2,0.4),
    c(2.1,0.6),
    c(2,0.3),
    c(2.1,0.1),
    c(2.3,0),
    c(-3,2),
    c(1.6, 3)
  )
}




#dbscan(get_more_complex_sample_data(), eps=0.05, minPts=0)


optics_r <- optics(get_more_complex_sample_data(), eps=0.3, minPts=2)

#data <- get_more_complex_sample_data()

#x <- data[1,]
#y <- data[2,]
#plot(x = data[1,], y = data[2,], xlim = c(-3,3), ylim= c(-3,3))

#x_1 <- data[1,which(optics_r$reachability<eps)]
#y_1 <- data[2,which(optics_r$reachability<eps)]

#plot(x = x_1, y = y_1, xlim = c(-3,3), ylim= c(-3,3), pch = 3)


plot_reachability <- function(optics_result = optics_r) {
  ordered_reachability <- optics_result$reachability[optics_result$ordered_list]
  ordered_reachability[ordered_reachability == Inf] <- 1
  n <- length(ordered_reachability)
  barplot(height = ordered_reachability, 
          width = (4/n), 
          space = 0, 
          xlim=c(0,4), 
          ylim=c(0,1))q
}

plot_reachability()



