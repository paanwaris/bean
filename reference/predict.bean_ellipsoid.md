# Predict suitability and Mahalanobis distance from a bean ellipsoid

Computes Mahalanobis distance and suitability values deriving from a
fitted `bean_ellipsoid` object for new environmental data.

## Usage

``` r
# S3 method for class 'bean_ellipsoid'
predict(
  object,
  newdata,
  include_suitability = TRUE,
  suitability_truncated = FALSE,
  include_mahalanobis = TRUE,
  mahalanobis_truncated = FALSE,
  keep_data = NULL,
  ...
)
```

## Arguments

- object:

  An object of class `bean_ellipsoid`.

- newdata:

  Environmental predictors. Can be a `data.frame`, `matrix`, or a
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).
  Must contain column/layer names matching the variables used to fit the
  ellipsoid.

- include_suitability:

  (logical) If `TRUE` (default), returns continuous suitability values.

- suitability_truncated:

  (logical) If `TRUE`, returns a truncated suitability layer where
  values outside the chi-square contour are set to `0`. Default =
  `FALSE`.

- include_mahalanobis:

  (logical) If `TRUE` (default), returns continuous Mahalanobis
  distance.

- mahalanobis_truncated:

  (logical) If `TRUE`, returns a truncated Mahalanobis layer where
  values outside the chi-square contour are set to `NA`. Default =
  `FALSE`.

- keep_data:

  (logical) If `TRUE`, includes the original predictors in the output.
  Default is `FALSE` for SpatRaster and `TRUE` for data.frames.

- ...:

  Additional arguments (unused).

## Value

A `data.frame` or `SpatRaster` (matching the input type of `newdata`)
containing the requested prediction layers.
