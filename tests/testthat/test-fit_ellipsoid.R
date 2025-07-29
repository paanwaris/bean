library(testthat)
library(dplyr)
library(ggplot2)

# --- Setup: Reusable Mock Data ---
# Create a predictable, pre-scaled dataset with a clear cluster and an outlier
# to test the robustness of the "mve" method.
set.seed(42)
mock_data_scaled <- data.frame(
  BIO1 = c(rnorm(50, mean = 0, sd = 1), 10), # 50 points in a cluster, 1 outlier
  BIO12 = c(rnorm(50, mean = 0, sd = 1), -10),
  species = "A"
)

# --- Test Suite ---

test_that("Core functionality works correctly (covmat method)", {
  # Run with the standard covariance method
  fit_covmat <- fit_ellipsoid(
    data = mock_data_scaled,
    env_vars = c("BIO1", "BIO12"),
    method = "covmat",
    level = 0.95 # Use 0-1 scale
  )

  # 1. Check object class and structure
  expect_s3_class(fit_covmat, "bean_ellipsoid")
  expect_named(fit_covmat, c(
    "niche_ellipse", "centroid", "covariance_matrix",
    "all_points_used", "points_in_ellipse", "points_outside_ellipse",
    "inside_indices", "parameters"
  ))

  # 2. Check that point counts are correct
  expect_equal(nrow(fit_covmat$points_in_ellipse) + nrow(fit_covmat$points_outside_ellipse),
               nrow(fit_covmat$all_points_used))

  # 3. Check that the centroid is influenced by the outlier (pulled away from 0)
  expect_true(abs(fit_covmat$centroid["BIO1"]) > 0.1)
  expect_true(fit_covmat$centroid["BIO12"] < -0.01) # Corrected logic
})


test_that("Core functionality works correctly (mve method)", {
  # Run with the robust MVE method
  fit_mve <- fit_ellipsoid(
    data = mock_data_scaled,
    env_vars = c("BIO1", "BIO12"),
    method = "mve",
    level = 0.95 # Use 0-1 scale
  )

  # 1. Check basic structure
  expect_s3_class(fit_mve, "bean_ellipsoid")
  expect_equal(nrow(fit_mve$points_in_ellipse) + nrow(fit_mve$points_outside_ellipse),
               nrow(fit_mve$all_points_used))

  # 2. Check that the centroid is robust to the outlier (should be very close to 0)
  expect_true(abs(fit_mve$centroid["BIO1"]) < 0.5)
  expect_true(abs(fit_mve$centroid["BIO12"]) < 0.5)
})


test_that("Input validation and error handling are robust", {
  # 1. Test invalid `env_vars`
  expect_error(fit_ellipsoid(mock_data_scaled, env_vars = "BIO1"),
               "`env_vars` must be a character vector of length two.")
  expect_error(fit_ellipsoid(mock_data_scaled, env_vars = c("BAD", "BIO12")),
               "Both variable names in `env_vars` must be present as columns in the data.")

  # 2. Test invalid `level`
  expect_error(fit_ellipsoid(mock_data_scaled, env_vars = c("BIO1", "BIO12"), level = 1.01),
               "`level` must be a single number greater than 0 and less than 1.")

  # 3. Test insufficient data
  small_data <- data.frame(BIO1 = c(1, 2), BIO12 = c(1, 2))
  expect_error(fit_ellipsoid(small_data, env_vars = c("BIO1", "BIO12")),
               "At least 3 complete observations are needed to fit an ellipse.")
})


test_that("S3 methods (print and plot) work as expected", {
  # Create one fit object to test both methods
  fit <- fit_ellipsoid(
    data = mock_data_scaled,
    env_vars = c("BIO1", "BIO12")
  )

  # 1. Test print method
  expect_output(print(fit), "--- Bean Environmental Niche Ellipse ---")
  expect_output(print(fit), "Method: 'covmat'.")
  expect_output(print(fit), "Fitted to 51 data points at a 95.00% level.")

  # 2. Test plot method
  p <- plot(fit)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Fitted Environmental Niche Ellipse")
  expect_equal(p$labels$x, "BIO1")
  expect_equal(p$labels$y, "BIO12")
})
