library(testthat)

test_that("thin_env_center returns correct structure and values", {
  mock_data <- data.frame(
    env1 = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4),
    env2 = c(10, 20, 20, 30, 30, 30, 40, 40, 40, 40),
    species = rep(c("A", "B"), 5)
  )

  # Each occupied cell: (env1/env2) pairs: (1/10), (2/20), (3/30), (4/40)
  # Deterministic thinning should return grid cell centers for each unique cell.

  result <- thin_env_center(
    data = mock_data,
    env_vars = c("env1", "env2"),
    grid_resolution = 1,
    verbose = FALSE
  )

  expect_s3_class(result, "bean_thinned_center")
  expect_named(result, c("thinned_points", "original_points"))
  expect_true(is.data.frame(result$thinned_points))
  expect_true(is.data.frame(result$original_points))
  expect_equal(ncol(result$thinned_points), 2)
  expect_equal(ncol(result$original_points), 2)
  # Should be 4 unique grid cells
  expect_equal(nrow(result$thinned_points), 4)
  # Original points should match input
  expect_equal(nrow(result$original_points), nrow(mock_data))

  # Check that all thinned points are at correct cell centers
  expected_centers <- data.frame(
    env1 = c(1.5, 2.5, 3.5, 4.5),
    env2 = c(10.5, 20.5, 30.5, 40.5)
  )
  expect_true(all(apply(result$thinned_points, 1, function(row) {
    any(apply(expected_centers, 1, function(exp_row) all(abs(row - exp_row) < 1e-8)))
  })))

  # Try with grid_resolution = c(2, 10)
  result2 <- thin_env_center(
    data = mock_data,
    env_vars = c("env1", "env2"),
    grid_resolution = c(2, 10),
    verbose = FALSE
  )
  # Only one center should be returned since all points fall into one cell for each axis
  expect_equal(nrow(result2$thinned_points), 4)

  # Handles non-finite input
  dirty_data <- rbind(
    mock_data,
    data.frame(env1 = NA, env2 = 50, species = "C"),
    data.frame(env1 = 5, env2 = Inf, species = "D")
  )
  result_dirty <- thin_env_center(
    data = dirty_data,
    env_vars = c("env1", "env2"),
    grid_resolution = 1,
    verbose = FALSE
  )
  expect_equal(nrow(result_dirty$original_points), 10)

  # All NA data returns empty result
  na_data <- data.frame(env1 = NA, env2 = NA, species = "E")
  result_na <- thin_env_center(
    data = na_data,
    env_vars = c("env1", "env2"),
    grid_resolution = 1,
    verbose = FALSE
  )
  expect_equal(nrow(result_na$thinned_points), 0)
})

test_that("thin_env_center errors with bad input", {
  mock_data <- data.frame(env1 = 1:5, env2 = 1:5)
  # wrong env_vars
  expect_error(thin_env_center(mock_data, c("foo", "bar"), 1))
  # wrong grid_resolution length
  expect_error(thin_env_center(mock_data, c("env1", "env2"), c(1, 2, 3)))
})

test_that("print.bean_thinned_center prints expected output", {
  mock_data <- data.frame(env1 = 1:5, env2 = 1:5)
  result <- thin_env_center(mock_data, c("env1", "env2"), 1, verbose = FALSE)
  expect_output(print(result), "Bean Deterministic Thinning Results")
})
