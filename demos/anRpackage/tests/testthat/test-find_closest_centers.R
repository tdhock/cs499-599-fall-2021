library(testthat)
test_that("error for ncol(data) != ncol(features)", {
  (data.mat <- matrix(1, 4, 3))
  (centers.mat <- matrix(1, 2, 2))
  expect_error({
    anRpackage::find_closest_center_interface(
      data.mat, centers.mat)
  }, "number of columns in data and centers should be the same")
})

test_that("output vector has reasonable size/values", {
  data.mat <- as.matrix(
    iris[, c("Petal.Length", "Petal.Width")])
  K <- 3
  set.seed(1)
  (centers.mat <- data.mat[sample(1:nrow(data.mat), K), ])
  (cluster.id.vec <-
    anRpackage::find_closest_center_interface(
      data.mat, centers.mat))
  expect_identical(length(cluster.id.vec), nrow(data.mat))
  expect_true(all(cluster.id.vec %in% 1:K))
})
