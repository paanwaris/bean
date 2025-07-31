# Load testthat and the function to be tested
library(testthat)
# source("R/fit_ellipsoid.R") # Or load the package with library(bean)

context("Testing fit_ellipsoid")

# --- 1. Test Input Validation and Error Handling ---

test_that("fit_ellipsoid stops with invalid inputs", {
  test_data <- data.frame(PC1 = 1:5, PC2 = 1:5, PC3 = 1:5)

  # Error: Invalid method
  expect_error(
    fit_ellipsoid(test_data, env_vars = c("PC1", "PC2"), method = "invalid_method"),
    "'arg' should be one of"
  )

  # Error: Invalid level
  expect_error(
    fit_ellipsoid(test_data, env_vars = c("PC1", "PC2"), level = 1.1),
    "`level` must be a single number greater than 0 and less than 1"
  )
  expect_error(
    fit_ellipsoid(test_data, env_vars = c("PC1", "PC2"), level = 0),
    "`level` must be a single number greater than 0 and less than 1"
  )

  # Error: env_vars has fewer than two variables
  expect_error(
    fit_ellipsoid(test_data, env_vars = "PC1"),
    "`env_vars` must be a character vector of at least length two"
  )

  # Error: env_vars not found in data
  expect_error(
    fit_ellipsoid(test_data, env_vars = c("PC1", "PC_X")),
    "One or more `env_vars` not found in the data frame"
  )

  # Error: Not enough complete observations
  expect_error(
    fit_ellipsoid(data.frame(PC1 = 1:2, PC2 = 1:2), env_vars = c("PC1", "PC2")),
    "At least n_variables \\+ 1 complete observations are needed"
  )
  expect_error(
    fit_ellipsoid(data.frame(PC1 = 1:3, PC2 = 1:3, PC3 = 1:3), env_vars = c("PC1", "PC2", "PC3")),
    "At least n_variables \\+ 1 complete observations are needed"
  )
})


# --- 2. Test 'covmat' Method ---

test_that("fit_ellipsoid with method='covmat' works correctly in 2D", {
  # Simple, centered data where calculations are predictable
  simple_data <- data.frame(
    V1 = c(0, 1, -1, 0, 0, 10), # includes an outlier
    V2 = c(0, 0, 0, 1, -1, 10)
  )
  env_vars <- c("V1", "V2")

  result <- fit_ellipsoid(simple_data, env_vars = env_vars, method = "covmat", level = 0.95)

  # Check centroid calculation (will be skewed by outlier)
  expect_equal(result$centroid, colMeans(simple_data))

  # Check covariance matrix
  expect_equal(result$covariance_matrix, stats::cov(simple_data))

  # Check point inclusion logic
  # The first 5 points should be inside, the outlier (10,10) should be outside
  expect_true(all(1:5 %in% result$inside_indices))
  expect_false(7 %in% result$inside_indices)
  expect_equal(nrow(result$points_in_ellipse), 6)
  expect_equal(nrow(result$points_outside_ellipse), 0)
})


# --- 3. Test 'mve' Method ---

test_that("fit_ellipsoid with method='mve' is robust to outliers", {
  # Data with a tight cluster and a clear outlier
  mve_data <- data.frame(
    V1 = c(rnorm(20, mean = 5, sd = 0.1), 50),
    V2 = c(rnorm(20, mean = 5, sd = 0.1), 50)
  )

  # Fit with both methods
  res_covmat <- fit_ellipsoid(mve_data, env_vars = c("V1", "V2"), method = "covmat")
  res_mve <- fit_ellipsoid(mve_data, env_vars = c("V1", "V2"), method = "mve")

  # The 'mve' centroid should be much closer to the true center (5,5) of the cluster
  # than the 'covmat' centroid, which is pulled toward the outlier (50,50).
  dist_covmat_to_center <- dist(rbind(res_covmat$centroid, c(5, 5)))
  dist_mve_to_center <- dist(rbind(res_mve$centroid, c(5, 5)))

  expect_lt(dist_mve_to_center, dist_covmat_to_center)

  # The 'mve' method should identify the outlier correctly
  # The index of the outlier is 21
  expect_false(21 %in% res_mve$inside_indices)
})


# --- 4. Test General Output Structure and Behavior ---

test_that("fit_ellipsoid returns a correctly structured object", {
  test_data <- data.frame(
    PC1 = c(rnorm(20, mean = 5, sd = 0.1), 50),
    PC2 = c(rnorm(20, mean = 5, sd = 0.1), 50)
  )
  result <- fit_ellipsoid(test_data, env_vars = c("PC1", "PC2"))

  # Check class and names
  expect_s3_class(result, "bean_ellipsoid")
  expect_named(result, c(
    "niche_ellipse", "centroid", "covariance_matrix", "all_points_used",
    "points_in_ellipse", "points_outside_ellipse", "inside_indices", "parameters"
  ))

  # Check that point subsets are correct and complete
  expect_equal(nrow(result$points_in_ellipse) + nrow(result$points_outside_ellipse), nrow(test_data))
  expect_equal(result$points_in_ellipse, result$all_points_used[result$inside_indices, ])
})

test_that("fit_ellipsoid handles NA values correctly", {
  na_data <- data.frame(
    V1 = c(rnorm(20, mean = 5, sd = 0.1), 50, NA),
    V2 = c(rnorm(20, mean = 5, sd = 0.1), NA, 50)
  )
  # Complete cases are rows 1, 3, 4, 5, 6 (5 rows total)

  result <- fit_ellipsoid(na_data, env_vars = c("V1", "V2"))

  # Check that all_points_used has filtered out rows with NAs
  expect_equal(nrow(result$all_points_used), 20)
  expect_false(any(is.na(result$all_points_used[, c("V1", "V2")])))
})

test_that("fit_ellipsoid generates correct ellipse object by dimension", {
  # Test 2D case
  data_2d <- data.frame(
    V1 = c(rnorm(20, mean = 5, sd = 0.1), 50, NA),
    V2 = c(rnorm(20, mean = 5, sd = 0.1), NA, 50)
  )
  res_2d <- fit_ellipsoid(data_2d, c("V1", "V2"))
  expect_true(is.data.frame(res_2d$niche_ellipse))

  # Test 3D case (requires rgl)
  if (requireNamespace("rgl", quietly = TRUE)) {
    data_3d <- data.frame(
      V1 = c(rnorm(20, mean = 5, sd = 0.1), 50, NA),
      V2 = c(rnorm(20, mean = 5, sd = 0.1), NA, 50),
      V3 = c(rnorm(20, mean = 5, sd = 0.1), NA, 50)
    )
    res_3d <- fit_ellipsoid(data_3d, c("V1", "V2", "V3"))
    expect_s3_class(res_3d$niche_ellipse, "mesh3d")
  } else {
    skip("rgl not installed, skipping 3D ellipse test.")
  }
})
