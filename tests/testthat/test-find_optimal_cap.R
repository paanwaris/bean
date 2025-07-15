# In tests/testthat/test-find_optimal_cap.R

test_that("find_optimal_cap returns correct caps and structure", {
  # Setup: create some dummy data with predictable clusters
  mock_data <- data.frame(
    temp_mean = c(1.1, 1.2, 1.3, 2.1, 2.2, 2.3, 2.4, 3.1),
    salinity_mean = c(10.1, 10.2, 10.3, 20.1, 20.2, 20.3, 20.4, 30.1)
  ) # Total 8 points. Clusters: 3, 4, 1. Max density is 4.

  # --- Test Case 1: Target is 75% (6 points) ---
  optimal_result_1 <- find_optimal_cap(
    data = mock_data,
    env_vars = c("temp_mean", "salinity_mean"),
    grid_resolution = 1,
    target_percent = 0.75 # Target is floor(8 * 0.75) = 6 points
  )
  # Search results will be: cap=1 -> 3 pts; cap=2 -> 5 pts; cap=3 -> 7 pts; cap=4 -> 8 pts
  # Closest to 6 is 5 (diff=1) or 7 (diff=1). Tie-break picks smaller cap.
  # So, 'closest' is cap=2 (retains 5).
  # 'Conservative' (>=6) are caps 3 and 4. Smallest is 3.
  # So, 'conservative' is cap=3 (retains 7).

  expect_type(optimal_result_1, "list")
  expect_named(optimal_result_1, c("best_cap_closest", "retained_points_closest",
                                   "best_cap_conservative", "retained_points_conservative",
                                   "search_results", "plot"))
  expect_equal(optimal_result_1$best_cap_closest, 2)
  expect_equal(optimal_result_1$retained_points_closest, 5)
  expect_equal(optimal_result_1$best_cap_conservative, 3)
  expect_equal(optimal_result_1$retained_points_conservative, 7)
  expect_s3_class(optimal_result_1$plot, "ggplot")
})

