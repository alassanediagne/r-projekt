get_2d_iris_test_data <- function() {
      iris |>
      dplyr::select(c(Petal.Length, Petal.Width)) |>
      data.matrix()
}


get_optics_result <- function() {
  optics(get_2d_iris_test_data(), 0.5, 3)
}

get_simple_test_data <- function() {
  rbind(
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


test_that("optics function executes", {
  data <- get_2d_iris_test_data()
  expect_no_error(optics(data, 0.5, 3))
  expect_no_warning(optics(data, 0.5, 3))
})

test_that("extract_dbscan function executes", {
  optics_result <- get_optics_result()
  expect_no_error(extract_dbscan(optics_result))
  expect_no_warning(extract_dbscan(optics_result))
})

test_that("plot_reachability function executes", {
  optics_result <- get_optics_result()
  expect_no_error(plot_reachability(optics_result))
  expect_no_warning(plot_reachability(optics_result))
})

test_that("plot_optics_2d function executes", {
  data <- get_2d_iris_test_data()
  optics_result <- get_optics_result()
  expect_no_error(plot_optics_2d(data, optics_result))
  expect_no_warning(plot_optics_2d(data, optics_result))
})

test_that("minPts has to be bigger than 1", {
  data <- get_2d_iris_test_data()
  expect_error(optics(data, eps = 1, minPts = 0))
})















