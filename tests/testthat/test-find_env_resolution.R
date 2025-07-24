library(testthat)
library(dplyr)
library(ggplot2)

# --- Setup: Reusable Mock Data ---
# Create a predictable, pre-scaled dataset.
# For BIO1, the distances will be 1, 2, and 3.
# For BIO12, the distances will be 10, 20, and 30.
mock_data_scaled <- data.frame(
  BIO1 = c(0, 1, 3, 6),
  BIO12 = c(0, 10, 30, 60),
  species = "A"
)

# --- Test Suite ---

test_that("Core functionality calculates correct resolutions", {
  # Run the function with a 50% quantile for predictable results
  # Distances for BIO1: 1, 3, 6, 2, 5, 3. Sorted: 1, 2, 3, 3, 5, 6. Median (50% quantile) = 3
  # Distances for BIO12: 10, 30, 60, 20, 50, 30. Sorted: 10, 20, 30, 30, 50, 60. Median (50% quantile) = 30
  resolutions <- find_env_resolution(
    data = mock_data_scaled,
    env_vars = c("BIO1", "BIO12"),
    quantile = 0.5
  )

  # 1. Check object class and names
  expect_s3_class(resolutions, "bean_resolution")
  expect_named(resolutions, c("suggested_resolution", "distance_distributions", "parameters"))

  # 2. Check the calculated resolutions
  expect_equal(resolutions$suggested_resolution[["BIO1"]], 3)
  expect_equal(resolutions$suggested_resolution[["BIO12"]], 30)

  # 3. Check the stored parameters
  expect_equal(resolutions$parameters$quantile, 0.5)
})


test_that("Input validation and edge cases are handled correctly", {
  # 1. Throws error on bad env_vars
  expect_error(
    find_env_resolution(mock_data_scaled, env_vars = c("BAD", "BIO12")),
    "One or both specified env_vars not found"
  )

  # 2. Throws error on insufficient data
  small_data <- data.frame(BIO1 = c(1, 2), BIO12 = c(1, 2))
  expect_error(
    find_env_resolution(small_data, env_vars = c("BIO1", "BIO12")),
    "At least 3 complete observations are needed to calculate resolution."
  )

  # 3. Handles data with NA values correctly (by ignoring them)
  mock_data_na <- mock_data_scaled
  mock_data_na$BIO1[1] <- NA

  # The function should run without error and produce a result based on the 3 valid rows
  expect_no_error(
    res_na <- find_env_resolution(mock_data_na, env_vars = c("BIO1", "BIO12"))
  )
  # Check that the result is based on the 3 complete cases
  expect_equal(length(res_na$distance_distributions$distances[res_na$distance_distributions$variable == "BIO1"]), 3)
})


test_that("S3 methods (print and plot) work as expected", {
  # Create a sample object to test the S3 methods
  sample_result <- list(
    suggested_resolution = c(BIO1 = 0.5, BIO12 = 1.5),
    distance_distributions = data.frame(
      variable = rep(c("BIO1", "BIO12"), each = 10),
      distances = runif(20)
    ),
    parameters = list(quantile = 0.1)
  )
  class(sample_result) <- "bean_resolution"

  # 1. Test print method
  expect_output(print(sample_result), "--- Bean Environmental Resolution Analysis ---")
  expect_output(print(sample_result), "Suggested Grid Resolutions \\(at the 10% quantile\\):")
  expect_output(print(sample_result), "- BIO1: 0.500000")

  # 2. Test plot method
  p <- plot(sample_result)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Distribution of Pairwise Environmental Distances per Axis")
  expect_equal(p$labels$x, "Environmental Distance (scaled)")
})
