data <- gen_clusters(50, matrix(c(0,1,2,0,1,2),ncol=2),0.3)

# Daten mit sehr geringem Lärm um (0,0), (1,0), (0,1), (1,1). Erwarte ungefähr diese Clusterzentren bei k-means mit k=3

test_that("Code funktioniert ohne Errors", {
  expect_no_error(k_means(data,4))
  expect_no_warning(k_means(data,4))
})

test_that("Richtige Clusteranzahl generiert", {
  expect_equal(nrow(k_means(data,4)$means),4)
})

test_that("Richtige Labelsanzahl generiert", {
  expect_equal(length(k_means(data,4)$labels), nrow(data))
})

test_that("Methode konvergiert für Testdaten", {
  expect_true(k_means(data,4)$converged)
})

