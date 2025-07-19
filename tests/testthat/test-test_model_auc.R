library(testthat)
library(raster)
library(dismo)
library(rJava)

test_that("test_model_auc returns correct structure and summary", {
  skip_if_not_installed("dismo")
  skip_if_not_installed("raster")

  # Create raster stack with all non-NA values and extent 1:10 x 1:10
  env <- raster::stack(
    raster::raster(matrix(1:100, 10, 10)),
    raster::raster(matrix(101:200, 10, 10))
  )
  names(env) <- c("bio1", "bio2")
  raster::extent(env) <- raster::extent(1, 10, 1, 10)

  # Sample points strictly within raster extent
  pres <- data.frame(lon = sample(1:10, 20, replace = TRUE), lat = sample(1:10, 20, replace = TRUE))
  back <- data.frame(lon = sample(1:10, 100, replace = TRUE), lat = sample(1:10, 100, replace = TRUE))

  # Filter points to raster extent (redundant here, but good practice)
  r_ext <- raster::extent(env)
  pres <- pres[
    pres$lon >= r_ext@xmin & pres$lon <= r_ext@xmax &
      pres$lat >= r_ext@ymin & pres$lat <= r_ext@ymax, ]
  back <- back[
    back$lon >= r_ext@xmin & back$lon <= r_ext@xmax &
      back$lat >= r_ext@ymin & back$lat <= r_ext@ymax, ]

  # Remove presences/backgrounds with NA raster values
  pres_vals <- raster::extract(env, pres)
  pres <- pres[complete.cases(pres_vals), ]
  back_vals <- raster::extract(env, back)
  back <- back[complete.cases(back_vals), ]

  suppressWarnings({
    result <- test_model_auc(
      presence_data = pres,
      background_data = back,
      env_rasters = env,
      longitude = "lon",
      latitude = "lat",
      k = 2,
      n_repeats = 1,
      verbose = FALSE
    )
  })

  expect_s3_class(result, "bean_evaluation")
  expect_named(result, c("summary", "all_auc_scores", "parameters"))
  expect_true(is.data.frame(result$summary))
  expect_true(is.numeric(result$all_auc_scores))
  expect_equal(length(result$all_auc_scores), result$parameters$k * result$parameters$n_repeats)
  expect_true(all(c("Mean_AUC", "SD_AUC", "Median_AUC", "Min_AUC", "Max_AUC") %in% names(result$summary)))
})

test_that("test_model_auc errors with missing columns", {
  skip_if_not_installed("dismo")
  skip_if_not_installed("raster")
  env <- raster::stack(
    raster::raster(matrix(runif(100), 10, 10))
  )
  raster::extent(env) <- raster::extent(1,10,1,10)
  pres <- data.frame(x = 1:5, y = 1:5)
  back <- data.frame(x = 1:10, y = 1:10)
  expect_error(test_model_auc(
    pres, back, env, longitude = "foo", latitude = "bar",
    k = 2, n_repeats = 1, verbose = FALSE
  ))
})

test_that("print.bean_evaluation prints expected output", {
  skip_if_not_installed("dismo")
  skip_if_not_installed("raster")
  env <- raster::stack(
    raster::raster(matrix(1:100, 10, 10))
  )
  raster::extent(env) <- raster::extent(1,10,1,10)
  pres <- data.frame(lon = sample(1:10, 10, replace = TRUE), lat = sample(1:10, 10, replace = TRUE))
  back <- data.frame(lon = sample(1:10, 20, replace = TRUE), lat = sample(1:10, 20, replace = TRUE))

  pres_vals <- raster::extract(env, pres)
  pres <- pres[complete.cases(pres_vals), ]
  back_vals <- raster::extract(env, back)
  back <- back[complete.cases(back_vals), ]

  suppressWarnings({
    result <- test_model_auc(
      presence_data = pres,
      background_data = back,
      env_rasters = env,
      longitude = "lon",
      latitude = "lat",
      k = 2,
      n_repeats = 1,
      verbose = FALSE
    )
  })
  expect_output(print(result), "Bean Model Evaluation Results")
})

test_that("plot.bean_evaluation returns a ggplot object", {
  skip_if_not_installed("dismo")
  skip_if_not_installed("raster")
  env <- raster::stack(
    raster::raster(matrix(1:100, 10, 10))
  )
  raster::extent(env) <- raster::extent(1,10,1,10)
  pres <- data.frame(lon = sample(1:10, 10, replace = TRUE), lat = sample(1:10, 10, replace = TRUE))
  back <- data.frame(lon = sample(1:10, 20, replace = TRUE), lat = sample(1:10, 20, replace = TRUE))

  pres_vals <- raster::extract(env, pres)
  pres <- pres[complete.cases(pres_vals), ]
  back_vals <- raster::extract(env, back)
  back <- back[complete.cases(back_vals), ]

  suppressWarnings({
    result <- test_model_auc(
      presence_data = pres,
      background_data = back,
      env_rasters = env,
      longitude = "lon",
      latitude = "lat",
      k = 2,
      n_repeats = 1,
      verbose = FALSE
    )
  })
  plt <- plot(result)
  expect_s3_class(plt, "ggplot")
})
