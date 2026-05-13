# Find objective environmental resolution using the Nearest Neighbor Elbow Method

Calculates an objective, data-driven grid resolution for environmental
thinning. Instead of relying on user-defined quantiles, this function
analyzes the 1D Nearest Neighbor (NN) distances for each environmental
variable. It uses the geometric "elbow" method (point of maximum
curvature) to identify the exact distance where dense artificial
clustering transitions into natural data spacing.

## Usage

``` r
find_env_resolution(data, env_vars)
```

## Arguments

- data:

  A data.frame containing environmental variables.

- env_vars:

  A character vector specifying the environmental variables to analyze.

## Value

An object of class `bean_env_resolution`
