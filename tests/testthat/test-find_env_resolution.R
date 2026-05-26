test_that("find_env_resolution returns one bandwidth per variable", {
  set.seed(1)
  df <- data.frame(a = rnorm(200), b = rnorm(200, sd = 3))
  res <- find_env_resolution(df, env_vars = c("a", "b"))

  expect_s3_class(res, "bean_env_resolution")
  expect_named(res$suggested_resolution, c("a", "b"))
  expect_true(all(res$suggested_resolution > 0))
  # Bandwidth for the wider variable should be larger.
  expect_gt(res$suggested_resolution[["b"]],
            res$suggested_resolution[["a"]])
})

test_that("find_env_resolution accepts all selector methods", {
  set.seed(2)
  df <- data.frame(x = rnorm(150))
  for (m in c("sheather-jones", "silverman", "scott")) {
    res <- find_env_resolution(df, env_vars = "x", method = m)
    expect_s3_class(res, "bean_env_resolution")
    expect_equal(res$method, m)
    expect_true(res$suggested_resolution[["x"]] > 0)
  }
})

test_that("find_env_resolution errors on bad input", {
  df <- data.frame(a = rnorm(10))
  expect_error(find_env_resolution(df, env_vars = "nope"),
               "not found")
  expect_error(find_env_resolution(df[1:3, , drop = FALSE], env_vars = "a"),
               "complete observations")
  expect_error(find_env_resolution(list(a = 1:10), env_vars = "a"),
               "data.frame")
})

test_that("find_env_resolution errors on a constant variable", {
  df <- data.frame(a = rnorm(50), b = rep(1, 50))
  expect_error(find_env_resolution(df, env_vars = c("a", "b")),
               "variability")
})
