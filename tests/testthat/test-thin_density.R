library(testthat)

test_that("thin_env_density returns an object of the correct class and size", {
  # Setup: create some dummy data
  mock_data <- data.frame(
    temp_mean = runif(100),
    salinity_mean = runif(100)
  )

  # Run the function
  thinned_result <- thin_env_density(
    data = mock_data,
    env_vars = c("temp_mean", "salinity_mean"),
    grid_resolution = 0.1,
    max_per_cell = 1
  )

  # Expectations:
  expect_s3_class(thinned_result, "bean_thinned_density")
  expect_true(thinned_result$n_thinned <= nrow(mock_data))
})

test_that("thin_env_density handles bad input gracefully", {
  # Expect an error if env_vars are not in data
  expect_error(
    thin_env_density(data = data.frame(x=1, y=1), env_vars = c("temp", "salt")),
    "not found in the data frame" # Optional: check for specific error message
  )
})
