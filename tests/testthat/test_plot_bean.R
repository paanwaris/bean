library(testthat)
library(dplyr)
library(ggplot2)
library(GGally)

# --- Setup: Reusable Mock Data and Objects ---

# 1. Create mock original data with 3 dimensions
mock_original_data_3d <- data.frame(
  PC1 = runif(100, -2, 2),
  PC2 = runif(100, -2, 2),
  PC3 = runif(100, -2, 2)
)

# 2. Create a mock stochastic thinned object
mock_thinned_density <- list(
  thinned_data = mock_original_data_3d[1:50, ],
  parameters = list(grid_resolution = c(0.5, 0.5, 0.5))
)
class(mock_thinned_density) <- "bean_thinned_density"

# 3. Create a mock deterministic thinned object
mock_thinned_center <- list(
  thinned_points = mock_original_data_3d[1:10, ], # Using a subset for the test
  parameters = list(grid_resolution = c(0.5, 0.5, 0.5))
)
class(mock_thinned_center) <- "bean_thinned_center"

# 4. Define mock environmental variables
mock_env_vars_3d <- c("PC1", "PC2", "PC3")


# --- Test Suite ---

test_that("Stochastic density plot is created correctly for n-dimensions", {
  # Skip if GGally is not installed
  skip_if_not_installed("GGally")

  p_stochastic <- plot_bean(
    original_data = mock_original_data_3d,
    thinned_object = mock_thinned_density,
    env_vars = mock_env_vars_3d
  )

  # 1. Check that the output is a ggpairs object
  expect_s3_class(p_stochastic, "ggmatrix")

  # 2. Check the plot title
  expect_equal(p_stochastic$title, "N-Dimensional Density Thinning")

  # 3. Check that the correct number of variables are plotted
  expect_equal(p_stochastic$ncol, 3)
  expect_equal(p_stochastic$nrow, 3)
})


test_that("Deterministic centroid plot is created correctly for n-dimensions", {
  # Skip if GGally is not installed
  skip_if_not_installed("GGally")

  p_deterministic <- plot_bean(
    original_data = mock_original_data_3d,
    thinned_object = mock_thinned_center,
    env_vars = mock_env_vars_3d
  )

  # 1. Check that the output is a ggpairs object
  expect_s3_class(p_deterministic, "ggmatrix")

  # 2. Check the plot title
  expect_equal(p_deterministic$title, "N-Dimensional Centroid Thinning")
})


test_that("Input validation works correctly", {
  # 1. Test with an invalid `thinned_object` class
  bad_object <- list()
  expect_error(
    plot_bean(mock_original_data_3d, bad_object, mock_env_vars_3d),
    "`thinned_object` must be an output from thin_env_nd() or thin_env_center()"
  )

  # 2. Test with missing `env_vars`
  bad_vars <- c("BAD", "VARS", "PC3")
  expect_error(
    plot_bean(mock_original_data_3d, mock_thinned_density, bad_vars),
    "One or more `env_vars` not found in `original_data` or the thinned data"
  )

  # 3. Test with thinned object missing grid_resolution parameter
  bad_thinned_object <- mock_thinned_density
  bad_thinned_object$parameters$grid_resolution <- NULL
  expect_error(
    plot_bean(mock_original_data_3d, bad_thinned_object, mock_env_vars_3d)
  )
})
