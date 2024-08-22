get_2d_iris_test_data <- function() {
      iris |>
      dplyr::select(c(Petal.Length, Petal.Width)) |>
      data.matrix()
}

get_optics_result <-function {
  optics(get_2d_iris_test_data(), 0.5, 3)
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















