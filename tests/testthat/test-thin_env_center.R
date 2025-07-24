library(testthat)
library(dplyr)

# --- Setup: Reusable Mock Data ---
# Create a predictable, pre-scaled dataset with clear clusters.
# With grid_resolution = 1, there are two occupied cells:
# - Cell "0_0" contains 2 points.
# - Cell "1_2" contains 3 points.
mock_data_scaled <- data.frame(
  BIO1 = c(0.1, 0.2, 1.1, 1.2, 1.3),
  BIO12 = c(0.1, 0.2, 2.1, 2.2, 2.3),
  species = "A"
)

# --- Test Suite ---

test_that("Core functionality calculates correct centroids", {
  # Run the function on the mock data
  thinned_result <- thin_env_center(
    data = mock_data_scaled,
    env_vars = c("BIO1", "BIO12"),
    grid_resolution = 1
  )

  # 1. Check object class and structure
  expect_s3_class(thinned_result, "bean_thinned_center")
  expect_named(thinned_result, c("thinned_points", "n_original", "n_thinned", "parameters"))

  # 2. Check the counts
  expect_equal(thinned_result$n_original, 5)
  expect_equal(thinned_result$n_thinned, 2) # Should find 2 unique cells

  # 3. Check that the calculated centers are correct
  # Cell 1 center: floor(0.1/1)*1 + 0.5 = 0.5; floor(0.1/1)*1 + 0.5 = 0.5
  # Cell 2 center: floor(1.1/1)*1 + 0.5 = 1.5; floor(2.1/1)*1 + 0.5 = 2.5
  expected_centers <- data.frame(
    BIO1 = c(0.5, 1.5),
    BIO12 = c(0.5, 2.5)
  )

  # Use arrange to ensure row order doesn't affect the test
  expect_equal(
    thinned_result$thinned_points %>% arrange(BIO1),
    expected_centers %>% arrange(BIO1)
  )
})


test_that("Input validation and error handling are robust", {
  # 1. Test invalid `env_vars`
  expect_error(
    thin_env_center(mock_data_scaled, env_vars = "", grid_resolution = 1),
    "One or both specified env_vars not found in the data frame."
  )
  expect_error(
    thin_env_center(mock_data_scaled, env_vars = c("BAD", "BIO12"), grid_resolution = 1),
    "One or both specified env_vars not found in the data frame."
  )

  # 2. Test invalid `grid_resolution`
  expect_error(
    thin_env_center(mock_data_scaled, env_vars = c("BIO1", "BIO12"), grid_resolution = c(1, 2, 3)),
    "grid_resolution must be a numeric vector of length 1 or 2."
  )

  # 3. Test with empty data frame
  empty_data <- data.frame(BIO1 = numeric(0), BIO12 = numeric(0))
  expect_message(
    result_empty <- thin_env_center(empty_data, c("BIO1", "BIO12"), grid_resolution = 1),
    "No complete observations to process."
  )
  expect_equal(result_empty$n_thinned, 0)
})


test_that("S3 print method works as expected", {
  # Create a sample object to test the print method
  sample_result <- list(
    thinned_points = data.frame(BIO1 = 0.5, BIO12 = 0.5),
    n_original = 10,
    n_thinned = 1,
    parameters = list()
  )
  class(sample_result) <- "bean_thinned_center"

  # Check for key phrases in the output
  expect_output(print(sample_result), "--- Bean Deterministic Thinning Results ---")
  expect_output(print(sample_result), "Thinned 10 original points to 1 unique grid cell centers.")
})

