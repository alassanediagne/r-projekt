data <- matrix(ncol=2, nrow=200)
data[1:50,1] <- seq(-0.1,0.1,length.out=50)
data[1:50,2] <- seq(0.1,-0.1,length.out=50)
data[51:100,1] <- seq(0.9,1.1,length.out=50)
data[51:100,2] <- seq(0.1,-0.1,length.out=50)
data[101:150,1] <- seq(-0.1,0.1,length.out=50)
data[101:150,2] <- seq(1.1,0.9,length.out=50)
data[151:200,1] <- seq(0.9,1.1,length.out=50)
data[151:200,1] <- seq(1.1,0.9,length.out=50)

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

