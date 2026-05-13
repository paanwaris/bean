# Fitted origin ellipsoid

A multidimensional ellipsoid fitted to the unthinned, prepared
occurrence data representing the baseline environmental niche of the
species.

## Usage

``` r
origin_ellipse
```

## Format

An object of class `bean_ellipsoid` containing:

- centroid:

  The multidimensional mean of the environmental variables

- covariance_matrix:

  The covariance matrix of the variables

- volume:

  The hypervolume of the fitted ellipsoid

- parameters:

  Model parameters including the confidence level (e.g., 0.95)
