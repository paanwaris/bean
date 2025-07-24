library(testthat)
library(dplyr)
library(ggplot2)

# --- Mock Dependencies ---
# A minimal `thin_env_density` is required for `find_optimal_cap` to call.
# This mock function isolates the test to the logic of the parent function.
# NOTE: This mock does NOT have a 'scale' argument.
thin_env_density <- function(data, env_vars, grid_resolution, max_per_cell) {
  env_var1_sym <- rlang::sym(env_vars[1])
  env_var2_sym <- rlang::sym(env_vars[2])

  thinned_data <- data %>%
    dplyr::mutate(
      env_cell_id = paste(
        floor(!!env_var1_sym / grid_resolution[1]),
        floor(!!env_var2_sym / grid_resolution[2]),
        sep = "_"
      )
    ) %>%
    dplyr::group_by(env_cell_id) %>%
    # slice_sample requires a seed for reproducibility in tests
    dplyr::slice_sample(n = min(n(), max_per_cell), replace = FALSE) %>%
    dplyr::ungroup()

  return(list(thinned_data = thinned_data))
}


# --- Setup: Reusable Mock Data ---
# Create a predictable, pre-scaled dataset.
# Total 10 points. Densities: 5 points in cell "0_0", 5 points in cell "1_1".
set.seed(81)
mock_data_scaled <- data.frame(
  BIO1 = c(rep(0.1, 5), rep(1.1, 5)),
  BIO12 = c(rep(0.2, 5), rep(1.2, 5))
)

# --- Test Suite ---

test_that("Core functionality calculates correct caps", {
  # Target 80% of 10 points = 8 points.
  # Search: cap=1->2pts, cap=2->4pts, cap=3->6pts, cap=4->8pts, cap=5->10pts
  # 'closest' to 8 is cap=4.
  # 'above_target' is also cap=4.
  optimal_result <- find_optimal_cap(
    data = mock_data_scaled,
    env_vars = c("BIO1", "BIO12"),
    grid_resolution = 1,
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
    data = mock_data_scaled,
    env_vars = c("BIO1", "BIO12"),
    grid_resolution = 1,
    target_percent = 0.95
  )

  expect_equal(optimal_default$parameters$target_point_count, 9)
  expect_equal(optimal_default$best_cap_closest, 4)
  expect_equal(optimal_default$best_cap_above_target, 5)
})


test_that("Input validation and error handling are robust", {
  # 1. Test invalid `env_vars`
  expect_error(
    find_optimal_cap(mock_data_scaled, env_vars = c("BIO10"), grid_resolution = 1,
                     target_percent = 0.95),
    "One or both specified env_vars not found in the data frame." # This error is from thin_env_density mock
  )

  # 2. Test invalid `grid_resolution`
  expect_error(
    find_optimal_cap(mock_data_scaled, env_vars = c("BIO1", "BIO12"), grid_resolution = c(1, 2, 3), target_percent = 0.95),
    "grid_resolution must be a numeric vector of length 1 or 2."
  )

  # 3. Test insufficient data
  small_data <- data.frame(BIO1 = c("NA"), BIO12 = c("NA"))
  expect_error(
    find_optimal_cap(small_data, env_vars = c("BIO1", "BIO12"), grid_resolution = 1, target_percent = 0.95),
    "No complete observations to run optimization."
  )
})


test_that("S3 methods (print and plot) work as expected", {
  # Create one fit object to test both methods
  fit <- find_optimal_cap(
    data = mock_data_scaled,
    env_vars = c("BIO1", "BIO12"),
    grid_resolution = 1,
    target_percent = 0.95
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
