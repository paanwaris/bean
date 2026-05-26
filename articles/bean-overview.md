# bean: an overview

`bean` reduces sampling bias in species occurrence data by thinning it
in **environmental space** rather than in geographic space. The result
is a cleaner training set for species distribution models (SDM / ENM).

The protocol is:

1.  **Prepare** raw occurrences with
    [`prepare_bean()`](https://paanwaris.github.io/bean/reference/prepare_bean.md).
2.  **Choose a grid resolution** with
    [`find_env_resolution()`](https://paanwaris.github.io/bean/reference/find_env_resolution.md),
    which selects a kernel-density bandwidth for each environmental
    variable.
3.  **Thin** occurrences with
    [`thin_env_nd()`](https://paanwaris.github.io/bean/reference/thin_env_nd.md)
    (stochastic) or
    [`thin_env_center()`](https://paanwaris.github.io/bean/reference/thin_env_center.md)
    (deterministic).
4.  **Fit a niche ellipsoid** with
    [`fit_ellipsoid()`](https://paanwaris.github.io/bean/reference/fit_ellipsoid.md).
5.  **Predict suitability** with
    [`predict()`](https://rdrr.io/r/stats/predict.html) on the fitted
    ellipsoid.

``` r

library(bean)
```

## Quickstart

``` r

data(origin_dat_prepared, package = "bean")
env_vars <- c("bio_1", "bio_4", "bio_12", "bio_15")

# 1. Pick an objective grid resolution from the data
res <- find_env_resolution(origin_dat_prepared, env_vars = env_vars)
res
#> --- Bean environmental grid resolution ---
#> Bandwidth selector: sheather-jones
#> 
#>  variable  resolution
#>     bio_1 0.056162684
#>     bio_4 0.013438324
#>    bio_12 0.004615848
#>    bio_15 0.006501067

# 2. Thin in environmental space
thinned <- thin_env_nd(
  data = origin_dat_prepared,
  env_vars = env_vars,
  grid_resolution = res$suggested_resolution,
  seed = 1
)
thinned
#> --- Bean Stochastic Thinning Results ---
#> 
#> Thinned 1024 original points to 78 points.
#> This represents a retention of 7.6% of the data.
#> 
#> --------------------------------------
```

The remaining vignettes walk through each step in detail.
