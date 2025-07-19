library(testthat)
library(raster)
library(dismo)
library(rJava)

test_that("test_model_auc runs correctly and returns the right structure", {

  # --- 1. Setup Mock Data ---
  # Create a very small mock raster stack
  r1 <- raster(nrows=10, ncols=10, vals=rnorm(100))
  r2 <- raster(nrows=10, ncols=10, vals=runif(100))
  mock_rasters <- stack(r1, r2)
  names(mock_rasters) <- c("bio1", "bio2")

  # Create mock presence and background data
  set.seed(1)
  mock_presence <- as.data.frame(randomPoints(mock_rasters, 50))
  colnames(mock_presence) <- c("lon", "lat")

  mock_background <- as.data.frame(randomPoints(mock_rasters, 100))
  colnames(mock_background) <- c("lon", "lat")

  # --- 2. Run the function with minimal settings for speed ---
  set.seed(2)
  eval_results <- test_model_auc(
    presence_data = mock_presence,
    background_data = mock_background,
    env_rasters = mock_rasters,
    longitude = "lon",
    latitude = "lat",
    k = 2,
    n_repeats = 1, # Use only 1 repeat to make the test fast
    verbose = FALSE
  )

  # --- 3. Check the Output ---
  # Check the class and structure of the returned object
  expect_s3_class(eval_results, "bean_evaluation")
  expect_named(eval_results, c("summary", "all_auc_scores", "parameters"))

  # Check that the number of AUC scores is correct (k * n_repeats)
  expect_length(eval_results$all_auc_scores, 2 * 1)

  # Check that the AUC scores are plausible values (between 0 and 1)
  expect_true(all(eval_results$all_auc_scores >= 0 & eval_results$all_auc_scores <= 1))

  # Check the summary table
  expect_s3_class(eval_results$summary, "data.frame")
  expect_equal(nrow(eval_results$summary), 1)
})

test_that("test_model_auc handles bad input", {
  # Create minimal data for testing errors
  mock_rasters <- stack(raster(nrows=1, ncols=1, vals=1))
  mock_presence <- data.frame(x = 1, y = 1)
  mock_background <- data.frame(x = 1, y = 1)

  # Should throw an error if the longitude/latitude columns don't exist
  expect_error(
    test_model_auc(
      presence_data = mock_presence,
      background_data = mock_background,
      env_rasters = mock_rasters,
      longitude = "lon", # This column does not exist
      latitude = "lat"   # This column does not exist
    )
  )
})
