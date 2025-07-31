library(testthat)
library(dplyr)
library(ggplot2)

# --- Setup: Reusable Mock Data ---
# Create a predictable, pre-scaled dataset with 3 dimensions.
# For BIO1, distances are 1, 2, 3. Median = 2.
# For BIO12, distances are 10, 20, 30. Median = 20.
# For BIO5, distances are 100, 200, 300. Median = 200.
mock_data_scaled_3d <- data.frame(
  BIO1 = c(0, 1, 3),
  BIO12 = c(0, 10, 30),
  BIO5 = c(0, 100, 300),
  species = "A"
)

# --- Test Suite ---

test_that("Core n-dimensional functionality calculates correct resolutions", {
  # Run the function with a 50% quantile (median) for predictable results
  resolutions <- find_env_resolution(
    data = mock_data_scaled_3d,
    env_vars = c("BIO1", "BIO12", "BIO5"),
    quantile = 0.5
  )

  # 1. Check object class and structure
  expect_s3_class(resolutions, "bean_resolution")
  expect_named(resolutions, c("suggested_resolution", "distance_distributions", "parameters"))
  expect_length(resolutions$suggested_resolution, 3)

  # 2. Check the calculated resolutions
  expect_equal(resolutions$suggested_resolution[["BIO1"]], 2)
  expect_equal(resolutions$suggested_resolution[["BIO12"]], 20)
  expect_equal(resolutions$suggested_resolution[["BIO5"]], 200)

  # 3. Check the stored parameters
  expect_equal(resolutions$parameters$quantile, 0.5)
})


test_that("Input validation and edge cases are handled correctly", {
  # 1. Throws error on bad env_vars
  expect_error(
    find_env_resolution(mock_data_scaled_3d, env_vars = c("BAD", "BIO12", "BIO5")),
    "One or more `env_vars` not found in the data frame"
  )

  # 2. Throws error on insufficient env_vars
  expect_error(
    find_env_resolution(mock_data_scaled_3d, env_vars = "BIO1"),
    "`env_vars` must contain at least two variable names"
  )

  # 3. Throws error on insufficient data
  small_data <- data.frame(BIO1 = c(1, 2), BIO12 = c(1, 2), BIO5 = c(1, 2))
  expect_error(
    find_env_resolution(small_data, env_vars = c("BIO1", "BIO12", "BIO5")),
    "At least 3 complete observations are needed to calculate resolution"
  )
})


test_that("S3 methods (print and plot) work as expected for n-dimensions", {
  # Create a sample object to test the S3 methods
  sample_result <- list(
    suggested_resolution = c(PC1 = 0.5, PC2 = 1.5, PC3 = 2.5),
    distance_distributions = data.frame(
      variable = rep(c("PC1", "PC2", "PC3"), each = 10),
      distances = runif(30)
    ),
    parameters = list(quantile = 0.1)
  )
  class(sample_result) <- "bean_resolution"

  # 1. Test print method
  expect_output(print(sample_result), "--- Bean Environmental Resolution Analysis ---")
  expect_output(print(sample_result), "Suggested Grid Resolutions \\(at the 10% quantile\\):")
  expect_output(print(sample_result), "- PC1: 0.500000")
  expect_output(print(sample_result), "- PC3: 2.500000")

  # 2. Test plot method
  p <- plot(sample_result)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Distribution of Pairwise Environmental Distances per Axis")
  expect_equal(p$labels$x, "Environmental Distance (scaled)")
})

