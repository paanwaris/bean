library(testthat)
library(ggplot2)
library(dplyr)

# --- Setup: Reusable Mock Data and Objects ---

# 1. Create mock original data
mock_original_data <- data.frame(
  BIO1 = runif(50, -2, 2),
  BIO12 = runif(50, -2, 2)
)

# 2. Create a mock stochastic thinned object
mock_thinned_stochastic <- list(
  thinned_data = mock_original_data[1:25, ],
  n_original = 50,
  n_thinned = 25
)
class(mock_thinned_stochastic) <- "bean_thinned_density"

# 3. Create a mock deterministic thinned object
mock_thinned_deterministic <- list(
  thinned_points = data.frame(BIO1 = c(-1, 1), BIO12 = c(-1, 1)),
  n_original = 50,
  n_thinned = 2
)
class(mock_thinned_deterministic) <- "bean_thinned_center"

# 4. Define a mock grid resolution
mock_grid_res <- c(1.0, 1.0)
mock_env_vars <- c("BIO1", "BIO12")

# --- Test Suite ---

test_that("Stochastic plot is created correctly", {
  p_stochastic <- plot_bean(
    original_data = mock_original_data,
    thinned_object = mock_thinned_stochastic,
    grid_resolution = mock_grid_res,
    env_vars = mock_env_vars
  )

  # 1. Check that the output is a ggplot object
  expect_s3_class(p_stochastic, "ggplot")

  # 2. Check the plot labels and titles
  expect_equal(p_stochastic$labels$title, "Stochastic Thinning Comparison")
  expect_equal(p_stochastic$labels$subtitle, "25 points remaining (blue) from 50 original points (grey)")
  expect_equal(p_stochastic$labels$x, "BIO1 (scaled)")
})


test_that("Deterministic plot is created correctly", {
  p_deterministic <- plot_bean(
    original_data = mock_original_data,
    thinned_object = mock_thinned_deterministic,
    grid_resolution = mock_grid_res,
    env_vars = mock_env_vars
  )

  # 1. Check that the output is a ggplot object
  expect_s3_class(p_deterministic, "ggplot")

  # 2. Check the plot labels and titles
  expect_equal(p_deterministic$labels$title, "Deterministic Thinning to Grid Cell Centers")
  expect_equal(p_deterministic$labels$subtitle, "2 unique cell centers (orange crosses) from 50 original points (grey)")
  expect_equal(p_deterministic$labels$y, "BIO12 (scaled)")
})


test_that("Input validation works correctly", {
  # 1. Test with an invalid `thinned_object` class
  bad_object <- list()
  expect_error(
    plot_bean(mock_original_data, bad_object, mock_grid_res, mock_env_vars),
    "`thinned_object` must be an `bean_thinned_density` or `bean_thinned_center` object."
  )

  # 2. Test with an invalid `grid_resolution`
  expect_error(
    plot_bean(mock_original_data, mock_thinned_stochastic, grid_resolution = "NA", mock_env_vars),
    "`grid_resolution` must be a numeric vector of length 2."
  )

  # 3. Test with missing `env_vars`
  bad_vars <- c("BAD", "VARS")
  expect_error(
    plot_bean(mock_original_data, mock_thinned_stochastic, mock_grid_res, bad_vars),
    "One or both `env_vars` not found in `original_data`."
  )
})
