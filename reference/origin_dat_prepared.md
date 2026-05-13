# Prepared occurrence data

A dataset containing the cleaned and scaled occurrence records ready for
environmental thinning. Missing coordinates and points outside the
raster extent have been removed, and environmental data has been
extracted.

## Usage

``` r
origin_dat_prepared
```

## Format

A data frame containing coordinates and environmental variables:

- species:

  The species name

- x:

  Longitude coordinates

- y:

  Latitude coordinates

- bio_1:

  Scaled Annual Mean Temperature

- bio_4:

  Scaled Temperature Seasonality

- bio_12:

  Scaled Annual Precipitation

- bio_15:

  Scaled Precipitation Seasonality
