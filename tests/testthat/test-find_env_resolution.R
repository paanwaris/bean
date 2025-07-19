library(testthat)

test_that("find_env_resolution returns correct class and values for typical input", {
  set.seed(42)
  data <- data.frame(
    species = rep(c("A", "B"), each = 6),
    env1 = scale(rnorm(12)),
    env2 = scale(runif(12))
  )
  res <- find_env_resolution(data, env_vars = c("env1", "env2"), quantile = 0.1, verbose = FALSE)
  expect_s3_class(res, "bean_resolution")
  expect_named(res$suggested_resolution, c("env1", "env2"))
  expect_true(all(res$suggested_resolution > 0))
  expect_true(is.data.frame(res$distance_distributions))
  expect_equal(unique(res$distance_distributions$variable), c("env1", "env2"))
})

test_that("find_env_resolution removes non-finite values", {
  data <- data.frame(
    species = c("A", "B", "C", "D", "E"),
    env1 = c(1, 2, 3, 4, 5),
    env2 = c(2, 3, 4, 5, NA)
  )
  expect_warning(
    res <- find_env_resolution(data, env_vars = c("env1", "env2"), verbose = TRUE),
    "rows with non-finite values were removed"
  )
  expect_s3_class(res, "bean_resolution")
})

test_that("find_env_resolution errors with insufficient data", {
  data <- data.frame(
    species = c("A", "B"),
    env1 = c(1, 2),
    env2 = c(2, 3)
  )
  expect_error(
    find_env_resolution(data, env_vars = c("env1", "env2")),
    "At least 3 complete observations are needed"
  )
})

test_that("find_env_resolution errors when env_vars not in data", {
  data <- data.frame(
    species = c("A", "B", "C"),
    env1 = c(1, 2, 3),
    env2 = c(2, 3, 4)
  )
  expect_error(
    find_env_resolution(data, env_vars = c("env1", "env_missing")),
    "not found in the data frame"
  )
})

test_that("print.bean_resolution prints the expected output", {
  data <- data.frame(
    species = rep("A", 5),
    env1 = scale(rnorm(5)),
    env2 = scale(rnorm(5))
  )
  res <- find_env_resolution(data, env_vars = c("env1", "env2"), verbose = FALSE)
  expect_output(print(res), "Bean Environmental Resolution Analysis")
})

test_that("plot.bean_resolution returns a ggplot object", {
  data <- data.frame(
    species = rep("A", 5),
    env1 = scale(rnorm(5)),
    env2 = scale(rnorm(5))
  )
  res <- find_env_resolution(data, env_vars = c("env1", "env2"), verbose = FALSE)
  p <- plot(res)
  expect_s3_class(p, "ggplot")
})
