library(testthat)

test_that("thin_env_center works correctly and returns the right structure", {

  # --- 1. Setup Mock Data ---
  # Create data that falls into 3 specific grid cells with resolution = 1
  # Cell 1 (center 0.5, 10.5): 2 points
  # Cell 2 (center 1.5, 10.5): 1 point
  # Cell 3 (center 1.5, 11.5): 3 points
  mock_data <- data.frame(
    temp = c(0.2, 0.8, 1.1, 1.6, 1.7, 1.9),
    precip = c(10.3, 10.6, 10.9, 11.2, 11.4, 11.8),
    x = 1:6, # extra column to check preservation
    y = 1:6
  )

  # --- 2. Run the function ---
  thinned_results <- thin_env_center(
    data = mock_data,
    env_vars = c("temp", "precip"),
    grid_resolution = 1,
    verbose = FALSE
  )

  # --- 3. Check the Output ---
  # Check the class and structure of the returned object
  expect_s3_class(thinned_results, "bean_thinned_center")
  expect_named(thinned_results, c("thinned_points", "original_points"))

  # Check that the number of thinned points is correct (3 unique cells)
  expect_equal(nrow(thinned_results$thinned_points), 3)

  # Check that the coordinates of the centroids are correct
  expected_centroids <- data.frame(
    temp = c(0.5, 1.5, 1.5),
    precip = c(10.5, 10.5, 11.5)
  )
  # Order the results to ensure a stable comparison
  thinned_points_ordered <- thinned_results$thinned_points[order(thinned_results$thinned_points$temp, thinned_results$thinned_points$precip), ]

  expect_equal(thinned_points_ordered, expected_centroids, ignore_attr = TRUE)
})

test_that("thin_env_center works with vector resolution", {
  mock_data <- data.frame(temp = c(0.2, 0.8, 1.1), precip = c(10.3, 10.6, 20.9))

  # Should result in 2 centroids with resolution c(1, 10)
  # Cell 1: center 0.5, 15
  # Cell 2: center 1.5, 25
  thinned_results <- thin_env_center(mock_data, c("temp", "precip"), c(1, 10), FALSE)
  expect_equal(nrow(thinned_results$thinned_points), 2)

  expected_centroids <- data.frame(temp = c(0.5, 1.5), precip = c(15, 25))
  thinned_points_ordered <- thinned_results$thinned_points[order(thinned_results$thinned_points$temp), ]
  expect_equal(thinned_points_ordered, expected_centroids, ignore_attr = TRUE)
})


test_that("thin_env_center handles bad input gracefully", {
  mock_data <- data.frame(x = 1:5, y = 1:5)

  # Throws error if env_vars are not in data
  expect_error(thin_env_center(mock_data, c("temp", "precip"), 1))

  # Throws error if grid_resolution has wrong length
  expect_error(thin_env_center(mock_data, c("x", "y"), c(1, 2, 3)))

  # Returns an empty object if no complete observations
  no_data <- data.frame(temp = c(1, NA), precip = c(NA, 2))
  empty_res <- thin_env_center(no_data, c("temp", "precip"), 1, FALSE)
  expect_s3_class(empty_res, "bean_thinned_center")
  expect_equal(nrow(empty_res$thinned_points), 0)
})
