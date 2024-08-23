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

test_that("k_medoids does not throw errors", {
  expect_no_error(k_medoids(data, 4))
  expect_no_warning(k_medoids(data, 4))
})

test_that("k_medoids generates the correct number of clusters", {
  result <- k_medoids(data, 4)
  expect_equal(length(unique(result$labels)), 4)
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
  expect_error(k_medoids(data, 100))
})

test_that("k_medoids handles edge cases with empty clusters", {
  result <- k_medoids(data, 4)
  expect_true(all(result$medoids != 0))
})
