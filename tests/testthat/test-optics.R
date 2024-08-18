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



optics(get_more_complex_sample_data(), eps=0.2, minPts=2)

