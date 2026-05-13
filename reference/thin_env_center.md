# Deterministic centroid

This function thins species occurrence records by finding all occupied
cells in a 2D environmental grid and returning a single new point at the
exact center of each of those cells. This is a deterministic method.

## Usage

``` r
thin_env_center(data, env_vars, grid_resolution)
```

## Arguments

- data:

  A data.frame containing species occurrence coordinates and the
  environmental variables.

- env_vars:

  A character vector specifying the column names in data that represent
  the environmental variables to be used in the analysis.

- grid_resolution:

  A numeric vector of length one or two specifying the resolution(s) for
  the grid axes. If length one, it is used for both axes.

## Value

An object of class `bean_thinned_center`. which is a list containing:

- thinned_points:

  A data.frame with two columns representing the new points at the
  center of each occupied environmental grid cell.

- n_original:

  An integer representing the number of complete occurrence records in
  the input data.

- n_thinned:

  An integer representing the number of unique grid cells that were
  occupied, which is also the number of points returned.

- parameters:

  A list of the key parameters used, such as whether scaling was
  applied.

## See also

[`thin_env_nd`](https://paanwaris.github.io/bean/reference/thin_env_nd.md),
[`find_env_resolution`](https://paanwaris.github.io/bean/reference/find_env_resolution.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Create environmental data
set.seed(81)
env_data <- data.frame(
BIO1 = c(0.1, 0.2, 1.1, 1.2, 1.3),
BIO12 = c(0.1, 0.2, 2.1, 2.2, 2.3)
)

# 2. Thin the data to grid cell centers
thinned_center_obj <- thin_env_center(
  data = env_data,
  env_vars = c("BIO1", "BIO12"),
  grid_resolution = c(0.1, 0.2)
)

# 3. Print the summary
print(thinned_center_obj)
} # }
```
