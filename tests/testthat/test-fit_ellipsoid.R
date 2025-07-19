library(testthat)

test_that("fit_ellipsoid works correctly with method = 'covmat'", {
  # Create mock data with some extra columns to check for preservation
  set.seed(1)
  mock_data <- data.frame(
    BIO1 = rnorm(100),
    BIO12 = rnorm(100, mean = 2),
    x = runif(100),
    y = runif(100)
  )

  # Run the function
  ellipse_covmat <- fit_ellipsoid(
    data = mock_data,
    var1 = "BIO1",
    var2 = "BIO12",
    method = "covmat",
    level = 95
  )

  # 1. Check the output object structure
  expect_s3_class(ellipse_covmat, "bean_ellipsoid")
  expect_named(ellipse_covmat, c(
    "centroid", "covariance_matrix", "level", "method",
    "niche_ellipse", "all_points_used", "points_in_ellipse",
    "points_outside_ellipse"
  ))

  # 2. Check the core logic
  # The number of inside and outside points should sum to the total
  expect_equal(
    nrow(ellipse_covmat$points_in_ellipse) + nrow(ellipse_covmat$points_outside_ellipse),
    nrow(ellipse_covmat$all_points_used)
  )

  # The number of points inside should be close to the specified level
  expect_true(nrow(ellipse_covmat$points_in_ellipse) > 85) # Should be ~95

  # 3. Check that extra columns are preserved
  expect_true(all(c("x", "y") %in% names(ellipse_covmat$points_in_ellipse)))
  expect_true(all(c("x", "y") %in% names(ellipse_covmat$points_outside_ellipse)))
})


test_that("fit_ellipsoid works correctly with method = 'mve'", {
  # Create mock data
  set.seed(1)
  mock_data <- data.frame(
    BIO1 = rnorm(100),
    BIO12 = rnorm(100, mean = 2)
  )

  # Run the function
  ellipse_mve <- fit_ellipsoid(
    data = mock_data,
    var1 = "BIO1",
    var2 = "BIO12",
    method = "mve",
    level = 95
  )

  # 1. Check the output object structure
  expect_s3_class(ellipse_mve, "bean_ellipsoid")

  # 2. Check the core logic for MVE
  # For MVE, the number of points inside should be exactly the quantile used
  expected_n_inside <- floor(100 * 0.95)
  expect_equal(nrow(ellipse_mve$points_in_ellipse), expected_n_inside)

  expect_equal(
    nrow(ellipse_mve$points_in_ellipse) + nrow(ellipse_mve$points_outside_ellipse),
    nrow(ellipse_mve$all_points_used)
  )
})


test_that("fit_ellipsoid handles bad input gracefully", {
  mock_data <- data.frame(x = 1:5, y = 1:5)

  # Throws error if var1/var2 are not in data
  expect_error(fit_ellipsoid(mock_data, "BIO1", "BIO12"))

  # Throws error with an invalid method
  expect_error(fit_ellipsoid(mock_data, "x", "y", method = "invalid_method"))

  # Throws error with too few points
  too_small_data <- data.frame(x = c(1,2), y = c(1,2))
  expect_error(fit_ellipsoid(too_small_data, "x", "y"))
})
