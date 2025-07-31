library(testthat)
library(dplyr)
library(ggplot2)

# --- Mock Dependencies ---
# A minimal `thin_env_nd` is required for `find_optimal_cap` to call.
# This mock function isolates the test to the logic of the parent function.
thin_env_nd <- function(data, env_vars, grid_resolution, max_per_cell) {
  # This mock doesn't need the full logic, just to return a predictable number of points
  # based on the cap. It simulates thinning by grouping and then sampling.
  env_data <- data[, env_vars, drop = FALSE]
  gridded_vals <- t(t(env_data) / grid_resolution)
  cell_ids <- apply(floor(gridded_vals), 1, paste, collapse = "_")

  thinned_data <- data %>%
    dplyr::mutate(env_cell_id = cell_ids) %>%
    dplyr::group_by(env_cell_id) %>%
    dplyr::slice_sample(n = min(n(), max_per_cell), replace = FALSE) %>%
    dplyr::ungroup()

  return(list(thinned_data = thinned_data))
}


# --- Setup: Reusable Mock Data ---
# Create a predictable, pre-scaled dataset with 3 dimensions.
# Total 10 points. Densities: 5 points in cell "0_0_0", 5 points in cell "1_1_1".
set.seed(42)
mock_data_scaled_3d <- data.frame(
  PC1 = c(rep(0.1, 5), rep(1.1, 5)),
  PC2 = c(rep(0.2, 5), rep(1.2, 5)),
  PC3 = c(rep(0.3, 5), rep(1.3, 5))
)

# --- Test Suite ---

test_that("Core n-dimensional functionality calculates correct caps", {
  # Target 80% of 10 points = 8 points.
  # Search: cap=1->2pts, cap=2->4pts, cap=3->6pts, cap=4->8pts, cap=5->10pts
  # 'closest' to 8 is cap=4.
  # 'above_target' is also cap=4.
  optimal_result <- find_optimal_cap(
    data = mock_data_scaled_3d,
    env_vars = c("PC1", "PC2", "PC3"),
    grid_resolution = c(1, 1, 1),
    target_percent = 0.80
  )

  # 1. Check object class and structure
  expect_s3_class(optimal_result, "bean_optimization")
  expect_named(optimal_result, c(
    "best_cap_closest", "retained_points_closest",
    "best_cap_above_target", "retained_points_above_target",
    "search_results", "parameters"
  ))

  # 2. Check calculated values
  expect_equal(optimal_result$best_cap_closest, 4)
  expect_equal(optimal_result$retained_points_closest, 8)
  expect_equal(optimal_result$best_cap_above_target, 4)
  expect_equal(optimal_result$retained_points_above_target, 8)
  expect_equal(optimal_result$parameters$target_point_count, 8)
})


test_that("Default target_percent works correctly", {
  # Default target is 95% of 10 points = 9 points.
  # Search: cap=1->2, cap=2->4, cap=3->6, cap=4->8, cap=5->10
  # 'closest' to 9 is cap=5 (diff 1), after tie-break with cap=4.
  # 'above_target' is cap=5.
  optimal_default <- find_optimal_cap(
    data = mock_data_scaled_3d,
    env_vars = c("PC1", "PC2", "PC3"),
    grid_resolution = c(1, 1, 1)
  )

  expect_equal(optimal_default$parameters$target_point_count, 9)
  expect_equal(optimal_default$best_cap_closest, 4)
  expect_equal(optimal_default$best_cap_above_target, 5)
})


test_that("Input validation and error handling are robust", {
  # 1. Test invalid `env_vars`
  expect_error(
    find_optimal_cap(mock_data_scaled_3d, env_vars = "PC1", grid_resolution = c(1, 1, 1)),
    "`env_vars` must contain at least two variable names"
  )

  # 2. Test mismatched `grid_resolution`
  expect_error(
    find_optimal_cap(mock_data_scaled_3d, env_vars = c("PC1", "PC2", "PC3"), grid_resolution = c(1, 1)),
    "Length of `grid_resolution` must match the length of `env_vars`"
  )

  # 3. Test insufficient data
  small_data <- data.frame(PC1 = 1, PC2 = 1, PC3 = NA)
  expect_error(
    find_optimal_cap(small_data, env_vars = c("PC1", "PC2", "PC3"), grid_resolution = c(1, 1, 1)),
    "No complete observations to run optimization."
  )
})


test_that("S3 methods (print and plot) work as expected", {
  # Create one fit object to test both methods
  fit <- find_optimal_cap(
    data = mock_data_scaled_3d,
    env_vars = c("PC1", "PC2", "PC3"),
    grid_resolution = c(1, 1, 1)
  )

  # 1. Test print method
  expect_output(print(fit), "--- Bean Optimization Results ---")
  expect_output(print(fit), "Target: Retain >= 9 occurrence points.")
  expect_output(print(fit), "Best Cap: 5")

  # 2. Test plot method
  p <- plot(fit)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Search for Optimal Density Cap")
  expect_equal(p$labels$x, "Maximum Points per Cell (Cap)")
})

