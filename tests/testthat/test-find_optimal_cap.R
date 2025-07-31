# Load testthat and the function to be tested
library(testthat)
# source("R/find_optimal_cap.R") # Or load the package with library(bean)

context("Testing find_optimal_cap")

# --- 1. Test Input Validation and Error Handling ---

test_that("find_optimal_cap stops with invalid inputs", {
  test_data <- data.frame(PC1 = 1:5, PC2 = 1:5)

  # Error: env_vars has fewer than two variables
  expect_error(
    find_optimal_cap(test_data, env_vars = "PC1", grid_resolution = 0.5),
    "`env_vars` must contain at least two variable names"
  )

  # Error: env_vars and grid_resolution length mismatch
  expect_error(
    find_optimal_cap(test_data, env_vars = c("PC1", "PC2"), grid_resolution = 0.5),
    "Length of `grid_resolution` must match the length of `env_vars`"
  )

  # Error: env_vars not found in data
  expect_error(
    find_optimal_cap(test_data, env_vars = c("PC1", "PC_X"), grid_resolution = c(0.5, 0.5)),
    "One or more `env_vars` not found in the data frame"
  )

  # Error: No complete observations
  na_data <- data.frame(PC1 = c(NA, NA), PC2 = c(NA, NA))
  expect_error(
    find_optimal_cap(na_data, env_vars = c("PC1", "PC2"), grid_resolution = c(0.5, 0.5)),
    "No complete observations to run optimization."
  )
})


# --- 2. Test Core Logic with Mocking ---

# Create a mock version of thin_env_nd.
# This mock function allows us to control the thinned count for any given cap.
# It takes the 'max_per_cell' (cap) and a predefined vector of results,
# and returns a list with a data.frame of the corresponding number of rows.
mock_thin_env_nd <- function(data, env_vars, grid_resolution, max_per_cell, result_vector) {
  thinned_count <- result_vector[max_per_cell]
  return(list(thinned_data = data.frame(matrix(nrow = thinned_count, ncol = 2))))
}

test_that("find_optimal_cap correctly identifies 'closest' and 'above target' caps", {
  # This dataset has 10 rows. We'll find the max density manually.
  # Cell IDs for res=1: 0_0 (4 times), 1_1 (3 times), 2_2 (3 times) -> max_density = 4
  test_data <- data.frame(
    V1 = c(0, 0.1, 0.2, 0.3, 1, 1.1, 1.2, 2, 2.1, 2.2),
    V2 = c(0, 0.1, 0.2, 0.3, 1, 1.1, 1.2, 2, 2.1, 2.2)
  )

  # Define the predictable results our mock function will produce for caps 1, 2, 3, 4
  # For 10 points and target_percent=0.8, the target count is 8.
  mock_results <- c(9, 8, 6, 4) # thinned counts for cap=1, 2, 3, 4

  # Use with_mocked_bindings to temporarily replace the real thin_env_nd
  # We pass our predefined 'result_vector' to our mock function.
  result <- with_mocked_bindings(
    thin_env_nd = function(...) mock_thin_env_nd(..., result_vector = mock_results),
    {
      find_optimal_cap(
        data = test_data,
        env_vars = c("V1", "V2"),
        grid_resolution = c(1, 1),
        target_percent = 0.8
      )
    }
  )

  # Target count is floor(10 * 0.8) = 8
  # Differences from target: abs(c(9, 8, 6, 4) - 8) -> c(1, 0, 2, 4)
  # The minimum difference is 0, which corresponds to cap = 2.
  expect_equal(result$best_cap_closest, 2)
  expect_equal(result$retained_points_closest, 8)

  # Counts >= target are 9 and 8. The function should pick the one that is
  # closest to the target, which is 8 (from cap = 2).
  expect_equal(result$best_cap_above_target, 2)
  expect_equal(result$retained_points_above_target, 8)
})

test_that("find_optimal_cap handles ties by choosing the smallest cap", {
  test_data <- data.frame(V1 = rep(0, 10), V2 = rep(0, 10)) # max_density = 10

  # For 10 points and target_percent=0.5, target is 5.
  # Let's create a tie for 'closest': counts of 6 and 4 are both diff=1 from target 5.
  # Let's create a tie for 'above': counts of 7 for caps 3 and 4.
  mock_results <- c(10, 8, 7, 7, 6, 4, 3, 2, 1, 1) # counts for caps 1..10

  result <- with_mocked_bindings(
    thin_env_nd = function(...) mock_thin_env_nd(..., result_vector = mock_results),
    {
      find_optimal_cap(
        data = test_data,
        env_vars = c("V1", "V2"),
        grid_resolution = c(1, 1),
        target_percent = 0.5
      )
    }
  )

  # Tie for closest: cap 5 (count 6) and cap 6 (count 4) are both diff=1.
  # The function should select the smaller cap, which is 5.
  expect_equal(result$best_cap_closest, 5)

  # Tie for above target: caps 3 and 4 both yield count=7.
  # The function should select the smaller cap, which is 3.
  expect_equal(result$best_cap_above_target, 5)
})

test_that("find_optimal_cap returns NA when no cap is above target", {
  test_data <- data.frame(V1 = rep(0, 10), V2 = rep(0, 10))

  # Target count is floor(10 * 0.9) = 9.
  # Let's provide mock results where no thinned count reaches the target.
  mock_results <- c(8, 7, 6, 5, 4, 3, 2, 1, 1, 1)

  result <- with_mocked_bindings(
    thin_env_nd = function(...) mock_thin_env_nd(..., result_vector = mock_results),
    {
      find_optimal_cap(
        data = test_data,
        env_vars = c("V1", "V2"),
        grid_resolution = c(1, 1),
        target_percent = 0.9
      )
    }
  )

  # Since no cap resulted in >= 9 points, these should be NA.
  expect_true(is.na(result$best_cap_above_target))
  expect_true(is.na(result$retained_points_above_target))

  # The 'closest' cap should still be found (cap=1 gives 8, diff=1).
  expect_equal(result$best_cap_closest, 1)
})


# --- 3. Test Output Structure ---

test_that("find_optimal_cap returns a correctly structured object", {
  test_data <- data.frame(V1 = c(0, 0.1, 1, 1.1), V2 = c(0, 0.1, 1, 1.1))
  # max_density = 2
  mock_results <- c(4, 2)

  result <- with_mocked_bindings(
    thin_env_nd = function(...) mock_thin_env_nd(..., result_vector = mock_results),
    {
      find_optimal_cap(test_data, env_vars = c("V1", "V2"), grid_resolution = c(1, 1))
    }
  )

  # Check class and names
  expect_s3_class(result, "bean_optimization")
  expect_named(result, c(
    "best_cap_closest", "retained_points_closest",
    "best_cap_above_target", "retained_points_above_target",
    "search_results", "parameters"
  ))

  # Check search_results data.frame
  expect_s3_class(result$search_results, "tbl_df")
  expect_named(result$search_results, c("cap", "thinned_count"))
  expect_equal(nrow(result$search_results), 2) # Should match max_density
})

