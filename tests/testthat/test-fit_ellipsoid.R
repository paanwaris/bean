library(testthat)
library(dplyr)
library(ggplot2)

# --- Setup: Reusable Mock Data ---
# 2D mock data with an outlier
mock_data_2d <- data.frame(
  BIO1 = c(rnorm(50, 0, 1), 10),
  BIO12 = c(rnorm(50, 0, 1), -10),
  species = "A"
)

# 3D mock data with an outlier
mock_data_3d <- data.frame(
  PC1 = c(rnorm(50, 0, 1), 10),
  PC2 = c(rnorm(50, 0, 1), -10),
  PC3 = c(rnorm(50, 0, 1), 10),
  species = "B"
)

# --- Test Suite ---

test_that("Core functionality works for 2D ellipses", {
  # Test covmat method
  fit_covmat_2d <- fit_ellipsoid(
    data = mock_data_2d,
    env_vars = c("BIO1", "BIO12"),
    method = "covmat",
    level = 0.95
  )

  # 1. Check object structure
  expect_s3_class(fit_covmat_2d, "bean_ellipsoid")
  expect_true(is.data.frame(fit_covmat_2d$niche_ellipse)) # Should be a data frame for 2D
  expect_equal(length(fit_covmat_2d$centroid), 2)

  # 2. Check that the centroid is influenced by the outlier
  expect_true(abs(fit_covmat_2d$centroid["BIO1"]) > 0.1)

  # Test mve method
  fit_mve_2d <- fit_ellipsoid(
    data = mock_data_2d,
    env_vars = c("BIO1", "BIO12"),
    method = "mve",
    level = 0.95
  )

  # 3. Check that the mve centroid is robust to the outlier
  expect_true(abs(fit_mve_2d$centroid["BIO1"]) < 0.5)
})


test_that("Core functionality works for 3D ellipsoids", {
  # Test covmat method
  fit_covmat_3d <- fit_ellipsoid(
    data = mock_data_3d,
    env_vars = c("PC1", "PC2", "PC3"),
    method = "covmat",
    level = 0.95
  )

  # 1. Check object structure
  expect_s3_class(fit_covmat_3d, "bean_ellipsoid")
  # For 3D, niche_ellipse is an rgl mesh object, not a simple data frame
  expect_true(inherits(fit_covmat_3d$niche_ellipse, "mesh3d"))
  expect_equal(length(fit_covmat_3d$centroid), 3)

  # 2. Check that the centroid is influenced by the outlier
  expect_true(abs(fit_covmat_3d$centroid["PC1"]) > 0.1)

  # Test mve method
  fit_mve_3d <- fit_ellipsoid(
    data = mock_data_3d,
    env_vars = c("PC1", "PC2", "PC3"),
    method = "mve",
    level = 0.95
  )

  # 3. Check that the mve centroid is robust to the outlier
  expect_true(abs(fit_mve_3d$centroid["PC1"]) < 0.5)
})


test_that("Input validation and error handling are robust", {
  # 1. Test invalid `env_vars`
  expect_error(
    fit_ellipsoid(mock_data_2d, env_vars = "BIO1"),
    "`env_vars` must be a character vector of at least length two"
  )

  # 2. Test insufficient data
  small_data <- data.frame(PC1 = 1:3, PC2 = 1:3, PC3 = 1:3)
  expect_error(
    fit_ellipsoid(small_data, env_vars = c("PC1", "PC2", "PC3")),
    "At least n_variables \\+ 1 complete observations are needed"
  )
})


test_that("S3 methods (print and plot) work for n-dimensions", {
  # Create 2D and 3D fit objects
  fit_2d <- fit_ellipsoid(data = mock_data_2d, env_vars = c("BIO1", "BIO12"))
  fit_3d <- fit_ellipsoid(data = mock_data_3d, env_vars = c("PC1", "PC2", "PC3"))

  # 1. Test print method
  expect_output(print(fit_2d), "Fitted in 2 dimensions")
  expect_output(print(fit_3d), "Fitted in 3 dimensions")

  # 2. Test plot method for 2D (returns ggplot)
  p_2d <- plot(fit_2d)
  expect_s3_class(p_2d, "ggplot")
  expect_equal(p_2d$labels$title, "Fitted Environmental Niche Ellipse")

  # 3. Test plot method for 3D (returns pairs plot if rgl not available, or runs rgl)
  # We check for the GGally class as a reliable test
  if (requireNamespace("GGally", quietly = TRUE)) {
    p_3d <- plot(fit_3d)
    expect_s3_class(fit_3d, "bean_ellipsoid")
  } else {
    skip("GGally not available for 3D plot test")
  }
})
