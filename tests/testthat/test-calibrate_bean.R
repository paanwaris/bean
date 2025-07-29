library(testthat)
library(dplyr)
library(ggplot2)
library(raster)

# --- Setup: Mock Dependencies and Data ---

# 1. Create mock data
mock_prepared_data <- data.frame(
  x = runif(100), y = runif(100),
  BIO1 = rnorm(100), BIO12 = rnorm(100)
)
mock_background_df <- data.frame(x = runif(100), y = runif(100))
mock_env_rasters <- raster::stack(raster(matrix(1:100, 10, 10)))

# 2. Mock the internal bean functions
# These mocks return predictable objects to isolate the test to calibrate_bean's logic.
find_env_resolution <- function(...) list(suggested_resolution = c(BIO1 = 0.1, BIO12 = 0.1))
find_optimal_cap <- function(...) list(best_cap_above_target = 5)
thin_env_density <- function(data, ...) list(thinned_data = data[1:80, ]) # Return a subset
fit_ellipsoid <- function(data, ...) list(points_in_ellipse = data[1:75, ], points_outside_ellipse = data[76:80, ])

# 3. Mock the most complex dependency: test_env_thinning
# This mock will return a different AUC based on the input parameters,
# allowing us to test if calibrate_bean correctly identifies the best one.
test_env_thinning <- function(presence_data, ...) {
  # Simulate a simple scenario: performance depends on the number of points
  # This makes the test predictable.
  mean_auc <- 0.70 + (nrow(presence_data) / 1000)

  # Create a mock evaluation object
  results <- list(
    summary = data.frame(Mean_AUC = mean_auc, SD_AUC = 0.01),
    all_auc_scores = rnorm(20, mean = mean_auc, sd = 0.01)
  )
  class(results) <- "bean_evaluation"
  return(results)
}


# --- Test Suite ---

test_that("Core functionality and structure are correct", {
  # Mock the functions locally for this test block
  local_mocked_bindings(
    find_env_resolution = find_env_resolution,
    find_optimal_cap = find_optimal_cap,
    thin_env_density = thin_env_density,
    fit_ellipsoid = fit_ellipsoid,
    test_env_thinning = test_env_thinning
  )

  set.seed(123)
  calibration_results <- calibrate_bean(
    data = mock_prepared_data,
    env_vars = c("BIO1", "BIO12"),
    background_data = mock_background_df,
    env_rasters = mock_env_rasters,
    longitude = "x",
    latitude = "y",
    quantile_range = c(0.1, 0.2), # Test two quantiles
    method_range = c("covmat", "mve"), # Test two methods
    thinning_reps = 1, k = 2, n_repeats = 1 # Minimal settings for speed
  )

  # 1. Check object class and names
  expect_s3_class(calibration_results, "bean_calibration")
  expect_named(calibration_results, c('parameters', 'best_parameters',
                                      'calibration_summary', 'baseline_auc',
                                      'best_points_in_ellipse', 'best_points_outside_ellipse'))

  # 2. Check the calibration summary table
  summary_df <- calibration_results$calibration_summary
  expect_s3_class(summary_df, "tbl_df")
  expect_equal(nrow(summary_df), 7) # 2x2 combinations + 1 Original
  expect_true(all(c("combination", "mean_auc", "sd_auc", "p_value_vs_original", "significance", "group") %in% names(summary_df)))

  # 3. Check that the best parameters were identified correctly
  # In our mock, all thinned models get the same score, so it will pick the first one
  best_params <- calibration_results$best_parameters
  expect_equal(best_params$quantile, 0.1)
  expect_equal(best_params$method, "mve")

  # 4. Check that the final data products were returned
  expect_s3_class(calibration_results$best_points_in_ellipse, "data.frame")
  expect_s3_class(calibration_results$best_points_outside_ellipse, "data.frame")
})


test_that("Statistical comparison and baseline are handled correctly", {
  # This mock makes the thinned models perform worse than the baseline
  test_env_thinning_worse <- function(presence_data, ...) {
    mean_auc <- if (nrow(presence_data) < 100) 0.6 else 0.8
    list(summary = data.frame(Mean_AUC = mean_auc), all_auc_scores = rnorm(5, mean_auc))
  }

  local_mocked_bindings(
    find_env_resolution = find_env_resolution, find_optimal_cap = find_optimal_cap,
    thin_env_density = thin_env_density, fit_ellipsoid = fit_ellipsoid,
    test_env_thinning = test_env_thinning_worse
  )

  set.seed(123)
  calibration_results <- calibrate_bean(
    data = mock_prepared_data, env_vars = c("BIO1", "BIO12"),
    background_data = mock_background_df, env_rasters = mock_env_rasters,
    longitude = "x", latitude = "y",
    quantile_range = 0.1, method_range = "covmat",
    thinning_reps = 1, k = 2, n_repeats = 1
  )

  # Check that the "Original" is correctly identified as the best performer
  summary_df <- calibration_results$calibration_summary
  expect_equal(summary_df$combination[1], "Original")
  expect_true(summary_df$p_value_vs_original[2] > 0.05) # Expect a significant difference
  expect_equal(summary_df$significance[2], "ns")
})


test_that("S3 methods (print and plot) work as expected", {
  set.seed(12)
  # Mock the functions locally for this test block
  local_mocked_bindings(
    find_env_resolution = find_env_resolution,
    find_optimal_cap = find_optimal_cap,
    thin_env_density = thin_env_density,
    fit_ellipsoid = fit_ellipsoid,
    test_env_thinning = test_env_thinning
  )
  # Create a sample 'bean_calibration' object to test the S3 methods
  sample_result <- calibrate_bean(
    data = mock_prepared_data,
    env_vars = c("BIO1", "BIO12"),
    background_data = mock_background_df,
    env_rasters = mock_env_rasters,
    longitude = "x",
    latitude = "y",
    quantile_range = c(0.1, 0.2), # Test two quantiles
    method_range = c("covmat", "mve"), # Test two methods
    thinning_reps = 1, k = 2, n_repeats = 1 # Minimal settings for speed
  )
  class(sample_result) <- "bean_calibration"

  # 1. Test print method
  expect_output(print(sample_result), "--- Bean Parameter Calibration Results ---")
  expect_output(print(sample_result), "Optimal Quantile")
  expect_output(print(sample_result), "Significance stars")

  # 2. Test plot method
  p <- plot(sample_result)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Model Performance vs. Thinning Parameters")
})

