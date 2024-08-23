library(testthat)

data <- rbind(
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


test_that("spectral_clustering does not throw errors", {
  expect_no_error(spectral_clustering(data, 4))
  expect_no_warning(spectral_clustering(data, 4))
})

test_that("spectral_clustering generates the correct number of projections", {
  result <- spectral_clustering(data, 4)
  expect_equal(length(result[,1]), 17)
})

test_that("k_means_spectral_clustering does not throw errors", {
  expect_no_error(k_means_spectral_clustering(data, 4))
  expect_no_warning(k_means_spectral_clustering(data, 4))
})

test_that("k_means_spectral_clustering generates correct number of clusters", {
  expect_equal(nrow(k_means_spectral_clustering(data,4)$means),4)
})

test_that("k_means_spectral_clustering assigns seperate clusters for outlier points",{
  labels <- k_means_spectral_clustering(data, 4)$labels
  for(i in 1:16){
    expect_true(labels[17]!=labels[i])
  }
  for(i in 1:15){
    expect_true(labels[16]!=labels[i])
  }
})

test_that("plot_k_means_spectral_clustering does not throw errors", {
  expect_no_error(plot_k_means_spectral_clustering(data, 4))
  expect_no_warning(plot_k_means_spectral_clustering(data, 4))
})
plot_k_means_spectral_clustering

data2 <- rbind(
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
  c(2.3,0)
)

test_that("plot_twoclusters_mean does not throw errors", {
  expect_no_error(plot_twoclusters_mean(data2))
  expect_no_warning(plot_twoclusters_mean(data2))
})

test_that("plot_twoclusters_mean assigns correct clusters",{
  labels <- plot_twoclusters_mean(data2)
  for(i in 1:8){
    expect_true(labels[1]==labels[i])
    expect_true(labels[9]!=labels[i])
  }
  for(i in 9:15){
    expect_true(labels[9]==labels[i])
  }
})
