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

# Daten mit sehr geringem Lärm um (0,0), (1,0), (0,1), (1,1). Erwarte ungefähr diese Clusterzentren bei k-means mit k=3

test_that("code does not throw errors", {
  expect_no_error(k_means(data,4))
  expect_no_warning(k_means(data,4))
})

test_that("correct number of clusters generated", {
  expect_equal(nrow(k_means(data,4)$means),4)
})

test_that("correct number of labels generated", {
  expect_equal(length(k_means(data,4)$labels), nrow(data))
})

test_that("method converges for test data", {
  expect_true(k_means(data,4)$converged)
})

test_that("throws error if too many clusters", {
  expect_error(k_means(data,100))
})

test_that("throws error of m0 and data have different dimensions", {
  expect_error(k_means(data,2, m0=matrix(c(1,2,3,4,5,6),ncol=3)))
})

test_that("throws error of m0 does not match num_cluster", {
  expect_error(k_means(data,4, m0=matrix(c(1,2,3,4,5,6),ncol=2)))
})

test_that("k_means_predict throws error if x is a function", {
  expect_error(k_means_predict(\(x) x, k_means(data)$means))
})

test_that("k_means_predict runs without error",{
  km <- k_means(data,4)$means
  expect_no_error(k_means_predict(c(1,2),km))
})

test_that("k_means_predict detects dimension mismatch",{
  km <- k_means(data,4)$means
  expect_error(k_means_predict(c(1,2,3),km))
})

test_that("k_means_predict works for matrix input", {
  km <- k_means(data,4)$means
  test_data <- rbind(c(1,3), c(4,4), c(-1,-7))
  expect_no_error(k_means_predict(test_data,km))
})
