not_noisy_data <- gen_clusters(50,matrix(c(0,1,2,0,1,2)), 0.01)
# Daten mit sehr geringem Lärm um (0,0), (1,1), (2,2). Erwarte diese Clusterzentren bei k-means mit k=3

test_that("Code funktioniert", {
  expect_no_error(k_means(not_noisy_data,3))
  expect_no_warning(k_means(not_noisy_data,3))
})

test_that("Richtige Clusteranzahl generiert", {
  expect_equal(nrow(k_means(not_noisy_data,3)),3)
})
