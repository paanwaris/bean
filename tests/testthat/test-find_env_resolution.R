library(testthat)

test_that("find_env_resolution works correctly", {
  # Create mock data with a predictable distance distribution
  mock_data <- data.frame(
    # Distances will be 1, 1, 2
    var1 = c(0, 1, 2),
    # Distances will be 10, 10, 20
    var2 = c(0, 10, 20)
  )

  # Run the function
  res <- find_env_resolution(mock_data, env_vars = c("var1", "var2"),
                             quantile = 0.5, verbose = FALSE) # Use 50th percentile (median)

  # 1. Check the output object structure
  expect_s3_class(res, "bean_resolution")
  expect_named(res, c("suggested_resolution", "distance_distributions"))

  # 2. Check the calculated resolutions
  # The median of (1, 1, 2) is 1
  # The median of (10, 10, 20) is 10
  expect_equal(res$suggested_resolution[["var1"]], 1)
  expect_equal(res$suggested_resolution[["var2"]], 10)

  # The resolution vector should be named
  expect_named(res$suggested_resolution, c("var1", "var2"))
})

test_that("find_env_resolution handles edge cases", {
  # Test that it stops with too few points
  too_small_data <- data.frame(var1 = c(1,2), var2 = c(1,2))
  expect_error(find_env_resolution(too_small_data, c("var1", "var2")))

  # Test that it stops if env_vars are not in the data
  mock_data <- data.frame(x = 1:5, y = 1:5)
  expect_error(find_env_resolution(mock_data, c("var1", "var2")))
})
