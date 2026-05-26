test_that("fit_ellipsoid (covmat) classifies points sensibly", {
  set.seed(81)
  df <- data.frame(
    BIO1  = c(rnorm(50, 10, 1), 30),
    BIO12 = c(rnorm(50, 20, 2), 50)
  )
  fit <- fit_ellipsoid(df, env_vars = c("BIO1", "BIO12"),
                       method = "covmat", level = 0.95)

  expect_s3_class(fit, "bean_ellipsoid")
  expect_equal(length(fit$centroid), 2L)
  expect_true(nrow(fit$points_in_ellipse) >= 45)
  # The far outlier should be flagged as outside.
  outliers <- fit$points_outside_ellipse
  expect_true(any(outliers$BIO1 > 25))
  # niche_ellipse polygon is filled for 2-D fits.
  expect_s3_class(fit$niche_ellipse, "data.frame")
  expect_equal(ncol(fit$niche_ellipse), 2L)
})

test_that("fit_ellipsoid (mve) is robust to outliers", {
  set.seed(82)
  df <- data.frame(
    a = c(rnorm(50), rnorm(5, 20, 1)),
    b = c(rnorm(50), rnorm(5, 20, 1))
  )
  fit <- fit_ellipsoid(df, env_vars = c("a", "b"),
                       method = "mve", level = 0.75)
  expect_s3_class(fit, "bean_ellipsoid")
  # Robust centre should be near the bulk of the data, not the outlier cluster.
  expect_lt(abs(fit$centroid[["a"]]), 3)
  expect_lt(abs(fit$centroid[["b"]]), 3)
})

test_that("fit_ellipsoid validates inputs", {
  df <- data.frame(a = rnorm(20), b = rnorm(20))
  expect_error(fit_ellipsoid(df, env_vars = "a"), "at least two")
  expect_error(fit_ellipsoid(df, env_vars = c("a", "b"), level = 1.2),
               "in \\(0, 1\\)")
  expect_error(fit_ellipsoid(df, env_vars = c("a", "z")), "not found")
})

test_that("predict.bean_ellipsoid returns expected columns", {
  set.seed(11)
  df <- data.frame(a = rnorm(100), b = rnorm(100))
  fit <- fit_ellipsoid(df, env_vars = c("a", "b"))
  newd <- data.frame(a = c(0, 5), b = c(0, 5))
  out  <- predict(fit, newdata = newd,
                  include_suitability = TRUE,
                  include_mahalanobis = TRUE,
                  keep_data = FALSE)
  expect_true(all(c("mahalanobis", "suitability") %in% names(out)))
  # Centre point should be much closer than the outlier.
  expect_lt(out$mahalanobis[1], out$mahalanobis[2])
  expect_gt(out$suitability[1], out$suitability[2])
})
