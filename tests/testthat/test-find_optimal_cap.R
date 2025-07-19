library(testthat)

test_that("find_optimal_cap returns correct caps and structure", {
  # Setup: create some dummy data with predictable clusters
  mock_data <- data.frame(
    temp = c(1.1, 1.2, 1.3, 2.1, 2.2, 2.3, 2.4, 3.1),
    precip = c(10.1, 10.2, 10.3, 20.1, 20.2, 20.3, 20.4, 30.1)
  ) # Total 8 points. Clusters: 3, 4, 1. Max density is 4.

  # --- Test Case 1: Target is 75% (6 points) ---
  # For reproducibility of the test itself
  set.seed(1)
  optimal_result_1 <- find_optimal_cap(
    data = mock_data,
    env_vars = c("temp", "precip"),
    grid_resolution = 1, # Single value
    target_percent = 0.75, # Target is floor(8 * 0.75) = 6 points
    verbose = FALSE
  )

  # Expected Search Results: cap=1 -> 3 pts; cap=2 -> 5 pts; cap=3 -> 7 pts; cap=4 -> 8 pts
  # 'closest' to target=6 is cap=2 (retains 5 pts, diff=1), after tie-break with cap=3.
  # 'above_target' (>=6) are caps 3 and 4. Closest to 6 is cap=3 (retains 7 pts).

  # Check the list names are correct
  expect_s3_class(optimal_result_1, "bean_optimization")
  expect_named(optimal_result_1, c(
    "best_cap_closest", "retained_points_closest",
    "best_cap_above_target", "retained_points_above_target",
    "search_results", "parameters"
  ))

  # Check the values in the returned list
  expect_equal(optimal_result_1$best_cap_closest, 2)
  expect_equal(optimal_result_1$retained_points_closest, 5)
  expect_equal(optimal_result_1$best_cap_above_target, 3)
  expect_equal(optimal_result_1$retained_points_above_target, 7)

  # --- Test Case 2: Using a vector for grid_resolution ---
  # Should produce the same result
  set.seed(1)
  optimal_result_2 <- find_optimal_cap(
    data = mock_data,
    env_vars = c("temp", "precip"),
    grid_resolution = c(1, 10), # Vector of two values
    target_percent = 0.75,
    verbose = FALSE
  )
  expect_equal(optimal_result_1$best_cap_closest, optimal_result_2$best_cap_closest)
})

test_that("find_optimal_cap handles edge cases and bad input", {
  mock_data <- data.frame(temp = 1:5, precip = 1:5)

  # Throws error if env_vars are not in data
  expect_error(find_optimal_cap(mock_data, c("BIO1", "BIO12"), 1, 0.5))

  # Throws error if grid_resolution has wrong length
  expect_error(find_optimal_cap(mock_data, c("temp", "precip"), c(1, 2, 3), 0.5))

  # Throws error with no complete observations
  no_data <- data.frame(temp = c(1, NA), precip = c(NA, 2))
  expect_error(find_optimal_cap(no_data, c("temp"), 1, 0.5))
})
