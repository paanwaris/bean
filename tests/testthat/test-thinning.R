make_df <- function(n = 200, seed = 1) {
  set.seed(seed)
  data.frame(
    a = c(rnorm(n / 2, 0, 0.1), rnorm(n / 2, 0, 1)),
    b = c(rnorm(n / 2, 0, 0.1), rnorm(n / 2, 0, 1))
  )
}

test_that("thin_env_nd retains one row per occupied cell", {
  df <- make_df()
  out <- thin_env_nd(df, env_vars = c("a", "b"),
                     grid_resolution = c(0.5, 0.5), seed = 42)

  expect_s3_class(out, "bean_thinned")
  expect_lte(out$n_thinned, out$n_original)
  expect_equal(nrow(out$thinned_data), out$n_thinned)
  # Each retained point must live in a distinct cell.
  cells <- apply(floor(out$thinned_data[, c("a", "b")] / 0.5), 1,
                 paste, collapse = "_")
  expect_equal(length(unique(cells)), length(cells))
})

test_that("thin_env_nd is reproducible with a seed and doesn't pollute globals", {
  df <- make_df()
  before <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL

  a <- thin_env_nd(df, env_vars = c("a", "b"),
                   grid_resolution = c(0.3, 0.3), seed = 7)
  b <- thin_env_nd(df, env_vars = c("a", "b"),
                   grid_resolution = c(0.3, 0.3), seed = 7)
  expect_equal(a$thinned_data, b$thinned_data)

  after <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  expect_identical(before, after)
})

test_that("thin_env_center returns the centre of every occupied cell", {
  df <- data.frame(a = c(0.1, 0.2, 1.1, 1.3),
                   b = c(0.1, 0.2, 1.1, 1.3))
  out <- thin_env_center(df, env_vars = c("a", "b"),
                         grid_resolution = 1)
  expect_s3_class(out, "bean_thinned_center")
  expect_equal(out$n_thinned, 2L)
  # Centres should be at (0.5, 0.5) and (1.5, 1.5).
  expect_true(all(c(0.5, 1.5) %in% out$thinned_points$a))
})

test_that("thinning functions validate input", {
  df <- make_df()
  expect_error(thin_env_nd(df, env_vars = c("a", "z"),
                           grid_resolution = c(0.1, 0.1)),
               "not found")
  expect_error(thin_env_nd(df, env_vars = c("a", "b"),
                           grid_resolution = 0.1),
               "match the length")
  expect_error(thin_env_center(df, env_vars = "x", grid_resolution = 1),
               "not found")
})
