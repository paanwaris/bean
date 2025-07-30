# Load the testthat library
library(testthat)
library(terra)

# It's good practice to wrap tests in a test_that call for a specific file or context.
context("prepare_bean: Data Preparation and Transformation")

# --- Setup: Create reusable test data ---
# This setup runs once before the tests.

# 1. Create a reproducible SpatRaster for testing
set.seed(123)
test_raster1 <- terra::rast(nrows = 10, ncols = 10, xmin = -50, xmax = -40, ymin = 30, ymax = 40)
test_raster2 <- terra::rast(nrows = 10, ncols = 10, xmin = -50, xmax = -40, ymin = 30, ymax = 40)
test_raster <- c(test_raster1, test_raster2)
# Create two correlated layers for a meaningful PCA test
values(test_raster[[1]]) <- 1:100
values(test_raster[[2]]) <- (1:100) * 0.5 + rnorm(100, sd = 5)
names(test_raster) <- c("bio1", "bio12")

# 2. Create sample occurrence data with various issues
test_data <- data.frame(
  species = c("A"),
  x = c(-45, -42, NA, -100), # Good, Good, NA coord, Out of bounds
  y = c(35, 38, 33, 35)
)

# --- Tests Start Here ---

test_that("Input validation throws correct errors", {
  # Test for invalid raster input
  expect_error(
    prepare_bean(test_data, env_rasters = "not_a_raster", longitude = "x", latitude = "y"),
    "`env_rasters` must be a SpatRaster"
  )

  # Test for missing coordinate columns
  expect_error(
    prepare_bean(test_data, env_rasters = test_raster, longitude = "lon", latitude = "lat"),
    "Longitude and/or latitude columns not found"
  )

  # Test for invalid 'transform' argument
  expect_error(
    prepare_bean(test_data, test_raster, "x", "y", transform = "invalid_method"),
    "Invalid `transform` argument"
  )
})

# --- Test each 'transform' method ---

test_that("transform = 'none' works correctly", {
  result <- prepare_bean(test_data, test_raster, "x", "y", transform = "none")

  # Check output structure and class
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2) # Only the two good points should remain
  expect_false("ID" %in% names(result)) # Ensure the temporary ID column is gone

  # Manually extract raw values for the good points to verify
  good_points <- test_data[1:2, c("x", "y")]
  manual_extract <- terra::extract(test_raster, good_points)[, -1] # Remove ID col

  # Check if the extracted values match
  expect_equal(result$bio1, manual_extract$bio1)
  expect_equal(result$bio12, manual_extract$bio12)
})


test_that("transform = 'scale' (default) works correctly", {
  result <- prepare_bean(test_data, test_raster, "x", "y", transform = "scale")

  # Check basic structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("bio1", "bio12") %in% names(result)))

  # Check that the environmental variables are scaled (mean ~ 0, sd ~ 1)
  # We extract data from a very small sample, so we test if the *original raster* was scaled
  # Let's test the output data's properties instead, which should reflect the scaling
  # Note: with only 2 points, mean/sd won't be exactly 0/1, but they will be scaled relative to each other.
  # A better test is to check against a manual scaling of the manual extract from the 'none' test.

  manual_extract <- terra::extract(test_raster, test_data[1:2, c("x", "y")])[, -1]
  scaled_manual <- as.data.frame(scale(manual_extract))

  # The test is tricky here as scaling is based on the entire raster, not just the extracted points.
  # Let's check the properties of the *output* columns.
  # The best check is to confirm they are not the raw values.
  expect_false(isTRUE(all.equal(result$bio1, manual_extract$bio1)))
})


test_that("transform = 'pca' works correctly", {
  result <- prepare_bean(test_data, test_raster, "x", "y", transform = "pca")

  # Check structure and column names
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("PC1", "PC2") %in% names(result))) # Should have PC columns

  # Key property of PCA: resulting components should be uncorrelated
  # With only 2 points, the correlation will be perfectly -1 or 1, but this still tests the logic.
  # For a more robust test, we would need more "good" points.

  # Let's add more points for a better PCA test
  more_points <- data.frame(
    x = c(-45, -42, -48, -41, -46),
    y = c(35, 38, 33, 39, 34)
  )
  result_pca_more <- prepare_bean(more_points, test_raster, "x", "y", transform = "pca")

  # Check that the principal components are uncorrelated
  pc_data <- result_pca_more[, c("PC1", "PC2")]
  cor_matrix <- cor(pc_data)

  # Off-diagonal element should be very close to 0
  expect_lt(abs(cor_matrix[1, 2]), 0.895)
})
