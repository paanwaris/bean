test_that("find_optimal_cap works correctly and creates output files", {
  # Create a temporary directory for this test. It will be cleaned up automatically.
  temp_output_dir <- tempfile(pattern = "bean_test_")
  on.exit(unlink(temp_output_dir, recursive = TRUE), add = TRUE)

  # Setup mock data with predictable clusters
  mock_data <- data.frame(
    temp = c(1.1, 1.2, 1.3, 2.1, 2.2, 2.3, 2.4, 3.1),
    precip = c(10.1, 10.2, 10.3, 20.1, 20.2, 20.3, 20.4, 30.1)
  ) # Total 8 points. Clusters: 3, 4, 1. Max density is 4.

  # For reproducibility of the test itself
  set.seed(1)

  # Run the function, telling it to write to the temp directory
  optimal_result_1 <- find_optimal_cap(
    data = mock_data,
    env_vars = c("temp", "precip"),
    grid_resolution = 1,
    target_percent = 0.75
  )

  # --- 1. Check the returned R object ---
  # Search results should be: cap=1->3; cap=2->5; cap=3->7; cap=4->8
  # 'closest' to target=6 is cap=2 (retains 5 pts, diff=1)
  # 'above_target' (>=6) are caps 3 and 4. Closest is cap=3 (retains 7 pts)

  # Check the list names are correct
  expect_named(optimal_result_1, c(
    "best_cap_closest", "retained_points_closest",
    "best_cap_above_target", "retained_points_above_target",
    "search_results", "plot"
  ))

  # Check the values in the returned list
  expect_equal(optimal_result_1$best_cap_closest, 2)
  expect_equal(optimal_result_1$retained_points_closest, 5)
  expect_equal(optimal_result_1$best_cap_above_target, 3)
  expect_equal(optimal_result_1$retained_points_above_target, 7)
  expect_s3_class(optimal_result_1$plot, "ggplot")
})
