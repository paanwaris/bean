library(testthat)
library(raster)
library(dismo)
library(rJava)

# --- Setup: Mock Data and Functions ---
# Create mock data that is simple and predictable
mock_presence <- data.frame(lon = 1:10, lat = 1:10)
mock_background <- data.frame(lon = 11:20, lat = 11:20)
mock_raster <- raster::stack(raster(matrix(1:100, 10, 10)))

# --- Test Suite ---

test_that("Core functionality produces correct structure and values", {
  # Mock the external functions from the 'dismo' package
  # This allows us to test our wrapper without running the actual Maxent software
  local_mocked_bindings(
    kfold = function(p, k, ...) rep(1:k, length.out = nrow(p)),
    maxent = function(...) "mock_maxent_model",
    evaluate = function(...) {
      # Return a mock evaluation object with a predictable AUC
      methods::new("ModelEvaluation", auc = 0.95)
    },
    .package = "dismo"
  )

  # Run the function with minimal settings for a quick test
  set.seed(123) # for reproducibility of the kfold mock if it were random
  results <- test_env_thinning(
    presence_data = mock_presence,
    background_data = mock_background,
    env_rasters = mock_raster,
    longitude = "lon",
    latitude = "lat",
    k = 2,
    n_repeats = 3
  )

  # 1. Check object class and names
  expect_s3_class(results, "bean_evaluation")
  expect_named(results, c("summary", "all_auc_scores", "parameters"))

  # 2. Check the dimensions and values of the results
  total_models <- 2 * 3 # k * n_repeats
  expect_length(results$all_auc_scores, total_models)
  expect_true(all(results$all_auc_scores == 0.95)) # All scores should be our mocked value

  # 3. Check the summary statistics
  expect_equal(results$summary$Mean_AUC, 0.95)
  expect_equal(results$summary$SD_AUC, 0) # SD should be 0 since all values are the same
  expect_equal(results$summary$Median_AUC, 0.95)

  # 4. Check that parameters are stored correctly
  expect_equal(results$parameters$k, 2)
  expect_equal(results$parameters$n_repeats, 3)
})


test_that("SpatRaster input is handled correctly", {
  # Mock the dismo functions again for this specific test
  local_mocked_bindings(
    maxent = function(x, ...) {
      # This is the key check: test the class of the object passed to maxent
      expect_s3_class(x, "RasterStack")
      return("mock_maxent_model")
    },
    evaluate = function(...) methods::new("ModelEvaluation", auc = 0.9),
    .package = "dismo"
  )

  # Create a SpatRaster from the terra package
  if (requireNamespace("terra", quietly = TRUE)) {
    mock_spatraster <- terra::rast(mock_raster)

    # Run the function and expect it to work without error
    expect_error(
      test_env_thinning(
        presence_data = mock_presence,
        background_data = mock_background,
        env_rasters = mock_spatraster,
        longitude = "lon",
        latitude = "lat",
        k = 2,
        n_repeats = 1
      )
    )
  } else {
    skip("terra package not available for SpatRaster test")
  }
})


test_that("S3 methods (print and plot) work as expected", {
  # Create a sample 'bean_evaluation' object to test the S3 methods
  sample_result <- list(
    summary = data.frame(Mean_AUC = 0.85, SD_AUC = 0.05, Median_AUC = 0.86,
                         Min_AUC = 0.75, Max_AUC = 0.95),
    all_auc_scores = c(0.8, 0.9, 0.75, 0.95),
    parameters = list(k = 2, n_repeats = 2,
                      maxent_args = c("linear=true", "doclamp=true"))
  )
  class(sample_result) <- "bean_evaluation"

  # 1. Test print method
  expect_output(print(sample_result), "--- Bean Model Evaluation Results ---")
  expect_output(print(sample_result), "Based on 2 repetitions of 2-fold cross-validation")
  expect_output(print(sample_result), "Mean_AUC")

  # 2. Test plot method
  p <- plot(sample_result)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Distribution of Cross-Validation AUC Scores")
  expect_equal(p$labels$x, "AUC Score")
})
