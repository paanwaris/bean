test_that("prepare_bean works on the shipped sample data", {
  skip_if_not_installed("terra")
  occ_file <- system.file("extdata", "Rusa_unicolor.csv", package = "bean")
  env_file <- system.file("extdata", "thai_env.tif",     package = "bean")
  skip_if(!nzchar(occ_file) || !nzchar(env_file),
          "sample data not installed")

  occ <- read.csv(occ_file)
  env <- terra::rast(env_file)

  prepared <- prepare_bean(
    data        = occ,
    env_rasters = env,
    longitude   = "x",
    latitude    = "y",
    transform   = "scale"
  )

  expect_s3_class(prepared, "data.frame")
  expect_true(all(c("x", "y") %in% names(prepared)))
  expect_true(nrow(prepared) > 0)
  expect_false(any(is.na(prepared)))
})

test_that("prepare_bean validates its input", {
  expect_error(prepare_bean(data.frame(x = 1), env_rasters = NULL,
                            longitude = "x", latitude = "y"),
               "SpatRaster")
})
