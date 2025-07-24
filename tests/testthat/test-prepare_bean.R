library(testthat)
library(terra)
library(raster) # For testing RasterStack compatibility

# --- Setup: Reusable Mock Data ---
# 1. Create a mock raster environment
mock_rast <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
terra::values(mock_rast) <- 1:100
names(mock_rast) <- "BIO1"

# 2. Create mock raw occurrence data with various issues
mock_occ_raw <- data.frame(
  x = c(5, 6, 7, NA, 15), # Point 4 has NA coord, Point 5 is outside raster
  y = c(5, 6, NA, 8, 15), # Point 3 has NA coord
  species = "A"
)

# --- Test Suite ---

test_that("Core functionality: data is cleaned and scaled correctly", {
  # Run the function with the mock data
  prepared <- prepare_bean(
    data = mock_occ_raw,
    env_rasters = mock_rast,
    longitude = "x",
    latitude = "y"
  )

  # 1. Check the output type and structure
  expect_s3_class(prepared, "data.frame")
  expect_true("BIO1" %in% names(prepared))
  expect_false("ID" %in% names(prepared)) # Check that the ID column was removed

  # 2. Check that the correct number of rows were kept
  # Should keep only the 2 valid points (rows 1 and 2)
  expect_equal(nrow(prepared), 2)

  # 3. Check that the environmental variable is scaled
  expect_equal(mean(prepared$BIO1), 0.0344691)
  expect_equal(sd(prepared$BIO1), 0.21936)

  # 4. Check that original columns are preserved
  expect_true("species" %in% names(prepared))
  expect_equal(prepared$species, c("A", "A"))
})


test_that("Input validation and format handling work correctly", {
  # 1. Test that it accepts a RasterStack from the 'raster' package
  mock_rasterstack <- raster::stack(mock_rast)
  expect_no_error(
    prepare_bean(
      data = mock_occ_raw,
      env_rasters = mock_rasterstack,
      longitude = "x",
      latitude = "y"
    )
  )

  # 2. Test for error with invalid raster class
  expect_error(
    prepare_bean(mock_occ_raw, env_rasters = matrix(1:10), "x", "y"),
    "`env_rasters` must be a SpatRaster from 'terra' or a RasterStack from 'raster'."
  )

  # 3. Test for error with missing coordinate columns
  expect_error(
    prepare_bean(mock_occ_raw, mock_rast, longitude = "bad_lon", latitude = "y"),
    "Longitude and/or latitude columns not found in `data`."
  )
})


test_that("Edge cases are handled gracefully", {
  # 1. Test with a data frame where NO points are valid
  bad_data <- data.frame(x = c(NA, 100), y = c(NA, 100))

  # Expect messages about removal, but no error
  expect_message(
    result_empty <- prepare_bean(bad_data, mock_rast, "x", "y"),
    "1 records removed due to missing coordinates."
  )
  # The second message is suppressed here but would appear in console

  expect_s3_class(result_empty, "data.frame")
  expect_equal(nrow(result_empty), 0)

  # 2. Test with data that is already clean
  clean_data <- data.frame(x = 1:5, y = 1:5)
  expect_message(
    result_clean <- prepare_bean(clean_data, mock_rast, "x", "y"),
    "Data preparation complete. Returning 5 clean records."
  )
  expect_equal(nrow(result_clean), 5)
})
