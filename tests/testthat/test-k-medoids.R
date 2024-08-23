library(testthat)

set.seed(13)
data <- gen_clusters(50,matrix(c(0,0,1,1,0,1,0,1),ncol=2),deviation = 0.1)


test_that("k_medoids does not throw errors", {
  expect_no_error(k_medoids(data, 4))
  expect_no_warning(k_medoids(data, 4))
})

test_that("k_medoids generates the correct number of clusters", {
  result <- k_medoids(data, 4)
  expect_equal(nrow(result$medoids), 4)
})

test_that("k_medoids assigns labels to all data points", {
  result <- k_medoids(data, 4)
  expect_equal(length(result$labels), nrow(data))
})

test_that("k_medoids converges for test data", {
  result <- k_medoids(data, 4)
  expect_true(result$converged)
})

test_that("k_medoids throws an error if too many clusters are requested", {
  expect_error(k_medoids(data, 300))
})



