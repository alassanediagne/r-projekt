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

test_that("minPts has to be bigger than 1", {
  data <- get_two_point_data()

  expect_error(dbscan(data, eps = 1, minPts = 0))
})

test_that("dbscan on two nodes work", {
  data <- get_two_point_data()

  expect_equal(dbscan(data, eps = 1, minPts = 2), list(c(1, 2)))
  expect_equal(dbscan(data, eps = 0.8, minPts = 1), list(1, 2))
})

# TODO this test should not rely on the seq of the result
test_that("dbscan works on more complex data", {
  expect_equal(dbscan(get_more_complex_sample_data(), eps = 0.3, minPts = 3),
               list(c(1,2,3,4,5,6,7,8), c(9,10,11,13, 14, 15, 12)))
})

#TODO
test_that("dbscan works with 3 dimensions", {

})

#TODO
test_that("dbscan works in higer dimensions", {

})
