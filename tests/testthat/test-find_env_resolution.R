# Load testthat and the function to be tested
library(testthat)
# source("R/find_env_resolution.R") # Or load the package with library(bean)

context("Testing find_env_resolution")

# --- 1. Test Input Validation and Error Handling ---

test_that("find_env_resolution stops with invalid inputs", {
  test_data <- data.frame(PC1 = 1:5, PC2 = 1:5, PC3 = 1:5)

  # Error: env_vars has fewer than two variables
  expect_error(
    find_env_resolution(test_data, env_vars = "PC1"),
    "`env_vars` must contain at least two variable names"
  )

  # Error: env_vars not found in data
  expect_error(
    find_env_resolution(test_data, env_vars = c("PC1", "PC_nonexistent")),
    "One or more `env_vars` not found in the data frame"
  )

  # Error: Not enough complete observations
  na_data <- data.frame(PC1 = c(1, 2, NA), PC2 = c(1, NA, 3))
  expect_error(
    find_env_resolution(na_data, env_vars = c("PC1", "PC2")),
    "At least 3 complete observations are needed to calculate resolution"
  )

  # Error: Only two complete observations
  two_rows_data <- data.frame(PC1 = c(1, 2, 3), PC2 = c(1, 2, NA))
  expect_error(
    find_env_resolution(two_rows_data, env_vars = c("PC1", "PC2")),
    "At least 3 complete observations are needed to calculate resolution"
  )
})

# --- 2. Test Correctness of Calculations ---

test_that("find_env_resolution calculates correct resolutions", {
  # Create a simple, predictable dataset
  simple_data <- data.frame(V1 = c(0, 4, 10), V2 = c(10, 12, 20))

  # For V1, distances are: 4, 10, 6. Sorted: 4, 6, 10.
  # For V2, distances are: 2, 10, 8. Sorted: 2, 8, 10.

  # Test with quantile = 0.5 (median)
  res_median <- find_env_resolution(simple_data, env_vars = c("V1", "V2"), quantile = 0.5)
  expect_equal(res_median$suggested_resolution[["V1"]], 6)
  expect_equal(res_median$suggested_resolution[["V2"]], 8)

  # Test with quantile = 0 (minimum)
  res_min <- find_env_resolution(simple_data, env_vars = c("V1", "V2"), quantile = 0)
  expect_equal(res_min$suggested_resolution[["V1"]], 4)
  expect_equal(res_min$suggested_resolution[["V2"]], 2)

  # Test with quantile = 1 (maximum)
  res_max <- find_env_resolution(simple_data, env_vars = c("V1", "V2"), quantile = 1)
  expect_equal(res_max$suggested_resolution[["V1"]], 10)
  expect_equal(res_max$suggested_resolution[["V2"]], 10)
})


# --- 3. Test Output Structure ---

test_that("find_env_resolution returns a correctly structured S3 object", {
  test_data <- data.frame(PC1 = 1:5, PC2 = 6:10, PC3 = 11:15)
  env_vars <- c("PC1", "PC2", "PC3")
  quantile_val <- 0.2

  result <- find_env_resolution(test_data, env_vars = env_vars, quantile = quantile_val)

  # Check class
  expect_s3_class(result, "bean_resolution")

  # Check top-level list names
  expect_named(result, c("suggested_resolution", "distance_distributions", "parameters"))

  # Check 'suggested_resolution' component
  expect_true(is.numeric(result$suggested_resolution))
  expect_equal(names(result$suggested_resolution), env_vars)
  expect_length(result$suggested_resolution, 3)

  # Check 'distance_distributions' component
  expect_true(is.data.frame(result$distance_distributions))
  expect_named(result$distance_distributions, c("variable", "distances"))
  # For 5 points, there are choose(5, 2) = 10 distances per variable
  expect_equal(nrow(result$distance_distributions), 3 * 10)

  # Check 'parameters' component
  expect_true(is.list(result$parameters))
  expect_named(result$parameters, "quantile")
  expect_equal(result$parameters$quantile, quantile_val)
})


# --- 4. Test Specific Behaviors ---

test_that("find_env_resolution handles NA values correctly", {
  # Data with NAs, but still leaving 3 complete rows
  na_data <- data.frame(
    V1 = c(0, 4, 10, NA, 5),
    V2 = c(10, 12, 20, 30, NA)
  )

  # The function should only use the first three rows, which are complete cases
  # Expected data used: data.frame(V1 = c(0, 4, 10), V2 = c(10, 12, 20))
  # V1 distances: 4, 10, 6. Median = 6
  # V2 distances: 2, 10, 8. Median = 8

  result <- find_env_resolution(na_data, env_vars = c("V1", "V2"), quantile = 0.5)

  # Check if calculation is based on the 3 complete rows, not all 5 rows
  expect_equal(result$suggested_resolution[["V1"]], 6)
  expect_equal(result$suggested_resolution[["V2"]], 8)

  # Check that the number of distances reflects the filtered data
  # For 3 points, choose(3, 2) = 3 distances per variable
  expect_equal(nrow(result$distance_distributions), 2 * 3)
})

