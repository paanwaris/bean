library(testthat)
library(dplyr)

# --- Setup: Reusable Mock Data ---
# Create a predictable, pre-scaled dataset with clear clusters.
# With grid_resolution = 1, there are two occupied cells:
# - Cell "0_0" contains 5 points.
# - Cell "1_2" contains 3 points.
mock_data_scaled <- data.frame(
  BIO1 = c(rep(0.1, 5), rep(1.1, 3)),
  BIO12 = c(rep(0.2, 5), rep(2.2, 3)),
  species = "A"
)

# --- Test Suite ---

test_that("Core functionality thins data correctly", {
  # Set seed because the function is stochastic (uses slice_sample)
  set.seed(123)

  thinned_result <- thin_env_density(
    data = mock_data_scaled,
    env_vars = c("BIO1", "BIO12"),
    grid_resolution = 1,
    max_per_cell = 2
  )

  # 1. Check object class and structure
  expect_s3_class(thinned_result, "bean_thinned_density")
  expect_named(thinned_result, c("thinned_data", "n_original", "n_thinned"))

  # 2. Check the counts
  expect_equal(thinned_result$n_original, 8)
  # Expected thinned count: 2 from the first cell, 2 from the second cell = 4
  expect_equal(thinned_result$n_thinned, 4)
  expect_equal(nrow(thinned_result$thinned_data), 4)
})


test_that("Input validation and error handling are robust", {
  # 1. Test invalid `env_vars`
  expect_error(
    thin_env_density(mock_data_scaled, env_vars = "", grid_resolution = 1, max_per_cell = 1),
    "One or both specified env_vars not found in the data frame."
  )
  expect_error(
    thin_env_density(mock_data_scaled, env_vars = c("BAD", "BIO12"), grid_resolution = 1, max_per_cell = 1),
    "One or both specified env_vars not found in the data frame."
  )

  # 2. Test invalid `grid_resolution`
  expect_error(
    thin_env_density(mock_data_scaled, env_vars = c("BIO1", "BIO12"), grid_resolution = c(1, 2, 3), max_per_cell = 1),
    "grid_resolution must be a numeric vector of length 1 or 2."
  )

  # 3. Test with empty data frame
  empty_data <- data.frame(BIO1 = numeric(0), BIO12 = numeric(0))
  expect_message(
    result_empty <- thin_env_density(empty_data, c("BIO1", "BIO12"), grid_resolution = 1, max_per_cell = 1),
    "No complete observations to thin."
  )
  expect_equal(result_empty$n_thinned, 0)
})


test_that("S3 print method works as expected", {
  # Create a sample object to test the print method
  sample_result <- list(
    thinned_data = data.frame(),
    n_original = 100,
    n_thinned = 75,
    parameters = list()
  )
  class(sample_result) <- "bean_thinned_density"

  # Check for key phrases in the output
  expect_output(print(sample_result), "--- Bean Stochastic Thinning Results ---")
  expect_output(print(sample_result), "Thinned 100 original points to 75 points.")
  expect_output(print(sample_result), "This represents a retention of 75.0% of the data.")
})
