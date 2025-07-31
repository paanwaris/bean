library(testthat)
library(dplyr)

# --- Setup: Reusable Mock Data ---
# Create a predictable, pre-scaled dataset with 3 dimensions.
# Total 10 points. Densities: 5 points in cell "0_0_0", 5 points in cell "1_1_1".
mock_data_scaled_3d <- data.frame(
  PC1 = c(rep(0.1, 5), rep(1.1, 5)),
  PC2 = c(rep(0.2, 5), rep(1.2, 5)),
  PC3 = c(rep(0.3, 5), rep(1.3, 5)),
  species = "A"
)

# --- Test Suite ---

test_that("Core n-dimensional functionality thins data correctly", {
  # Set seed because the function is stochastic (uses slice_sample)
  set.seed(123)

  thinned_result <- thin_env_nd(
    data = mock_data_scaled_3d,
    env_vars = c("PC1", "PC2", "PC3"),
    grid_resolution = c(1, 1, 1),
    max_per_cell = 2
  )

  # 1. Check object class and structure
  expect_s3_class(thinned_result, "bean_thinned_density")
  expect_named(thinned_result, c("thinned_data", "n_original", "n_thinned", "parameters"))

  # 2. Check the counts
  expect_equal(thinned_result$n_original, 10)
  # Expected thinned count: 2 from the first cell, 2 from the second cell = 4
  expect_equal(thinned_result$n_thinned, 4)
  expect_equal(nrow(thinned_result$thinned_data), 4)

  # 3. Check that other columns are preserved
  expect_true("species" %in% names(thinned_result$thinned_data))
})


test_that("Input validation and error handling are robust", {
  # 1. Test invalid `env_vars`
  expect_error(
    thin_env_nd(mock_data_scaled_3d, env_vars = "PC1", grid_resolution = c(1), max_per_cell = 1),
    "`env_vars` must contain at least two variable names."
  )
  expect_error(
    thin_env_nd(mock_data_scaled_3d, env_vars = c("BAD", "PC2", "PC3"), grid_resolution = c(1, 1, 1), max_per_cell = 1),
    "One or more `env_vars` not found in the data frame."
  )

  # 2. Test mismatched `grid_resolution`
  expect_error(
    thin_env_nd(mock_data_scaled_3d, env_vars = c("PC1", "PC2", "PC3"), grid_resolution = c(1, 1), max_per_cell = 1),
    "Length of `grid_resolution` must match the length of `env_vars`."
  )

  # 3. Test with empty data frame
  empty_data <- data.frame(PC1 = numeric(0), PC2 = numeric(0), PC3 = numeric(0))
  expect_message(
    result_empty <- thin_env_nd(empty_data, c("PC1", "PC2", "PC3"), c(1, 1, 1), 1),
    "No complete observations to thin."
  )
  expect_equal(result_empty$n_thinned, 0)
})


test_that("S3 print method (inherited) works as expected", {
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

