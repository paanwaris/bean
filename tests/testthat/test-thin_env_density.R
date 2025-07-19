library(testthat)

test_that("thin_env_density returns correct structure and values", {
  set.seed(123)
  mock_data <- data.frame(
    env1 = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4),
    env2 = c(10, 20, 20, 30, 30, 30, 40, 40, 40, 40),
    species = rep(c("A", "B"), 5)
  )

  # Each cell: (env1/env2) pairs: (1/10), (2/20), (3/30), (4/40)
  # Counts: 1, 2, 3, 4

  # Cap at 2 per cell
  result <- thin_env_density(
    data = mock_data,
    env_vars = c("env1", "env2"),
    grid_resolution = 1,
    max_per_cell = 2,
    verbose = FALSE
  )

  expect_s3_class(result, "bean_thinned_density")
  expect_named(result, c("thinned_data", "n_original", "n_thinned"))
  expect_true(is.data.frame(result$thinned_data))
  expect_equal(result$n_original, 10)
  expect_true(result$n_thinned <= result$n_original)
  # Should have at most 2 points per cell
  tab <- table(
    paste(
      floor(result$thinned_data$env1 / 1),
      floor(result$thinned_data$env2 / 1),
      sep = "_"
    )
  )
  expect_true(all(tab <= 2))

  # Cap at 1 per cell
  result1 <- thin_env_density(
    data = mock_data,
    env_vars = c("env1", "env2"),
    grid_resolution = 1,
    max_per_cell = 1,
    verbose = FALSE
  )
  tab1 <- table(
    paste(
      floor(result1$thinned_data$env1 / 1),
      floor(result1$thinned_data$env2 / 1),
      sep = "_"
    )
  )
  expect_true(all(tab1 == 1))

  # Cap at 10 per cell (should retain all points)
  result10 <- thin_env_density(
    data = mock_data,
    env_vars = c("env1", "env2"),
    grid_resolution = 1,
    max_per_cell = 10,
    verbose = FALSE
  )
  expect_equal(result10$n_original, result10$n_thinned)

  # Check non-finite removal
  dirty_data <- rbind(
    mock_data,
    data.frame(env1 = NA, env2 = 50, species = "C"),
    data.frame(env1 = 5, env2 = Inf, species = "D")
  )
  result_dirty <- thin_env_density(
    data = dirty_data,
    env_vars = c("env1", "env2"),
    grid_resolution = 1,
    max_per_cell = 2,
    verbose = FALSE
  )
  expect_equal(result_dirty$n_original, 10)

  # All NA data returns empty result
  na_data <- data.frame(env1 = NA, env2 = NA, species = "E")
  result_na <- thin_env_density(
    data = na_data,
    env_vars = c("env1", "env2"),
    grid_resolution = 1,
    max_per_cell = 2,
    verbose = FALSE
  )
  expect_equal(result_na$n_thinned, 0)
})

test_that("thin_env_density errors with bad input", {
  mock_data <- data.frame(env1 = 1:5, env2 = 1:5)
  # wrong env_vars
  expect_error(thin_env_density(mock_data, c("foo", "bar"), 1, 2))
  # wrong grid_resolution length
  expect_error(thin_env_density(mock_data, c("env1", "env2"), c(1, 2, 3), 2))
})

test_that("print.bean_thinned_density prints expected output", {
  mock_data <- data.frame(env1 = 1:5, env2 = 1:5)
  result <- thin_env_density(mock_data, c("env1", "env2"), 1, 1, verbose = FALSE)
  expect_output(print(result), "Bean Stochastic Thinning Results")
})
