# Thin occurrence data in n-dimensional environmental space

This function thins species occurrence records in an n-dimensional
environmental space by randomly sampling exactly one point from each
occupied n-dimensional grid cell (hypercube).

## Usage

``` r
thin_env_nd(data, env_vars, grid_resolution, seed = NULL)
```

## Arguments

- data:

  A data.frame containing species occurrences and pre-scaled
  environmental variables, typically the output of \`prepare_bean()\`.

- env_vars:

  A character vector of two or more column names representing the
  environmental variables (dimensions) to use for thinning.

- grid_resolution:

  A numeric vector of resolutions for each environmental axis. Its
  length must match the length of \`env_vars\`.

- seed:

  (numeric) An optional random seed for reproducibility. If provided,
  the random number generator state is safely isolated to this function
  call and will not affect the global environment. Default = NULL.

## Value

An object of class \`bean_thinned\`, which is a list containing:

- thinned_data:

  A data.frame containing the occurrence records that were retained
  after the thinning process.

- n_original:

  An integer representing the number of complete occurrence records in
  the input data before thinning.

- n_thinned:

  An integer representing the number of occurrence records remaining
  after thinning.

- parameters:

  A list of the key parameters used during the thinning process.

## Examples

``` r
data(origin_dat_prepared, package = "bean")
thinned <- thin_env_nd(
  data            = origin_dat_prepared,
  env_vars        = c("bio_1", "bio_12"),
  grid_resolution = c(0.5, 0.5),
  seed            = 123
)
print(thinned)
#> --- Bean Stochastic Thinning Results ---
#> 
#> Thinned 1024 original points to 77 points.
#> This represents a retention of 7.5% of the data.
#> 
#> --------------------------------------
```
