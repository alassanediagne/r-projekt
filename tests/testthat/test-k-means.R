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

