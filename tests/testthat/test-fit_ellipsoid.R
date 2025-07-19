library(testthat)

test_that("fit_ellipsoid returns correct structure and values (covmat)", {
  set.seed(42)
  test_data <- data.frame(
    var1 = scale(rnorm(50)),
    var2 = scale(rnorm(50)),
    species = rep(c("A", "B"), 25)
  )
  fit <- fit_ellipsoid(
    data = test_data,
    var1 = "var1",
    var2 = "var2",
    method = "covmat",
    level = 95
  )
  expect_s3_class(fit, "bean_ellipsoid")
  expect_named(fit, c(
    "centroid", "covariance_matrix", "level", "method",
    "niche_ellipse", "all_points_used", "points_in_ellipse", "points_outside_ellipse"
  ))
  expect_true(is.data.frame(fit$niche_ellipse))
  expect_true(is.data.frame(fit$all_points_used))
  expect_true(is.data.frame(fit$points_in_ellipse))
  expect_true(is.data.frame(fit$points_outside_ellipse))
  expect_equal(length(fit$centroid), 2)
  expect_equal(dim(fit$covariance_matrix), c(2, 2))
  expect_true(nrow(fit$points_in_ellipse) + nrow(fit$points_outside_ellipse) == nrow(fit$all_points_used))
})

test_that("fit_ellipsoid returns correct structure and values (mve)", {
  set.seed(42)
  test_data <- data.frame(
    var1 = scale(rnorm(50)),
    var2 = scale(rnorm(50)),
    species = rep(c("A", "B"), 25)
  )
  fit <- fit_ellipsoid(
    data = test_data,
    var1 = "var1",
    var2 = "var2",
    method = "mve",
    level = 95
  )
  expect_s3_class(fit, "bean_ellipsoid")
  expect_true(is.data.frame(fit$niche_ellipse))
  expect_true(is.data.frame(fit$all_points_used))
  expect_true(is.data.frame(fit$points_in_ellipse))
  expect_true(is.data.frame(fit$points_outside_ellipse))
})

test_that("fit_ellipsoid errors with insufficient data", {
  small_data <- data.frame(var1 = c(1, NA, 3), var2 = c(NA, 2, 3))
  expect_error(fit_ellipsoid(small_data, "var1", "var2"))
})

test_that("fit_ellipsoid errors with missing columns", {
  test_data <- data.frame(a = rnorm(10), b = rnorm(10))
  expect_error(fit_ellipsoid(test_data, "foo", "bar"))
})

test_that("fit_ellipsoid errors with invalid method", {
  test_data <- data.frame(var1 = rnorm(10), var2 = rnorm(10))
  expect_error(fit_ellipsoid(test_data, "var1", "var2", method = "invalid"))
})

test_that("print.bean_ellipsoid prints expected output", {
  set.seed(42)
  test_data <- data.frame(
    var1 = scale(rnorm(50)),
    var2 = scale(rnorm(50))
  )
  fit <- fit_ellipsoid(test_data, "var1", "var2")
  expect_output(print(fit), "Bean Environmental Niche Ellipse")
})

test_that("plot.bean_ellipsoid returns a ggplot object", {
  set.seed(42)
  test_data <- data.frame(
    var1 = scale(rnorm(50)),
    var2 = scale(rnorm(50))
  )
  fit <- fit_ellipsoid(test_data, "var1", "var2")
  plt <- plot(fit)
  expect_s3_class(plt, "ggplot")
})
