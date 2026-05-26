# Plot method for `bean_env_resolution`

Draws one panel per environmental variable showing the kernel density
estimate used to derive the suggested grid resolution. The bandwidth is
marked as a horizontal scale bar at the bottom of each panel.

## Usage

``` r
# S3 method for class 'bean_env_resolution'
plot(x, ...)
```

## Arguments

- x:

  A `bean_env_resolution` object.

- ...:

  Additional graphical parameters passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly returns `NULL`.
