###############################################
### TODO Sorry for removing this but it brakes the package build

### Needs adaptation
### eg. move to vignette

### NEVER use library calls in the package code it messes up the namespaces
################################################

# library(tidyverse)
# source("/Users/alassanediagne/Documents/Uni/Master/SS24/R-Vorlesung/r-projekt/R/gen_clusters.R")
# source("/Users/alassanediagne/Documents/Uni/Master/SS24/R-Vorlesung/r-projekt/R/k-means.R")
#
# data <- gen_clusters(100, matrix(c(0,0,1,1,1,0,0,1), ncol=2),0.3)
# plot(data)
# plot_k_means_2d(data,4)
#
#
# ggplot() +
#   geom_point(data=iris, aes(x=Petal.Length, y = Petal.Width, color=Species)) +
#   theme_light()
#
# irisPetals <- iris |>
#   dplyr::select(c(Petal.Length, Petal.Width)) |>
#   data.matrix()
#
#
#
# plot_k_means_2d(irisPetals, 3)
#
# data <- gen_clusters(50, matrix(c(0,1,2,1,0,0,1,2,0,2),ncol=2), 0.4)
# plot(data)
# plot_k_means_2d(data, 5)
