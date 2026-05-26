# Plot a fitted `bean_ellipsoid`

Draws a 2-D ellipse using base R graphics, or a 3-D interactive
ellipsoid using rgl (a suggested dependency). If rgl is not installed
and a 3-D plot is requested, the function falls back to the 2-D view of
the first two dimensions and emits a
[`message()`](https://rdrr.io/r/base/message.html).

## Usage

``` r
# S3 method for class 'bean_ellipsoid'
plot(x, dims = c(1, 2), ..., window_size = c(800, 800))
```

## Arguments

- x:

  A `bean_ellipsoid` object.

- dims:

  Either a numeric vector of column indices or a character vector of
  variable names. Length 2 draws a 2-D plot, length \>=3 draws a 3-D
  plot using the first three.

- ...:

  Unused.

- window_size:

  Numeric length-2 size of the rgl window. Default `c(800, 800)`.

## Value

Invisibly `NULL`.
