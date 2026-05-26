# Cleaned and scaled occurrence data

Output of
[`prepare_bean`](https://paanwaris.github.io/bean/reference/prepare_bean.md)
applied to `occ_data_raw` using the bundled environmental rasters.
Missing coordinates and records outside the raster extent have been
removed, and environmental values have been extracted and standardised.

## Usage

``` r
origin_dat_prepared
```

## Format

A `data.frame` with the columns:

- species:

  Species name.

- x, y:

  Coordinates.

- bio_1:

  Scaled annual mean temperature.

- bio_4:

  Scaled temperature seasonality.

- bio_12:

  Scaled annual precipitation.

- bio_15:

  Scaled precipitation seasonality.
