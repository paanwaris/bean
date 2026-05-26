# Prepare data for environmental thinning

This function serves as a pre-processing step to clean and prepare
species occurrence data. It performs three key actions: 1. Removes
records with missing longitude or latitude values. 2. Extracts
environmental data from raster layers that are already scaled for each
occurrence point. 3. Removes records that fall outside the raster extent
or have missing environmental data. The final output is a clean data
frame where the environmental variables have a mean of 0 and a standard
deviation of 1.

## Usage

``` r
prepare_bean(
  data,
  env_rasters,
  longitude,
  latitude,
  transform = c("scale", "pca", "none")
)
```

## Arguments

- data:

  A data.frame of species occurrences records, including columns for
  longitude and latitude.

- env_rasters:

  A SpatRaster (from `terra` package) or RasterStack (from `raster`
  package) object of environmental variables.

- longitude:

  (character) The name of the longitude column in `data`.

- latitude:

  (character) The name of the latitude column in `data`.

- transform:

  (character) The transformation to apply to the environmental rasters
  before extracting data. Options are "scale" (default), "pca", or
  "none". See Details.

## Value

A data.frame containing the cleaned and scaled occurrence data, with the
following columns:

- Original Columns:

  All columns from the input `data` are preserved for the valid records.

- Environmental Variables:

  New columns, named after the layers in `env_rasters`, containing the
  extracted and scaled environmental data.

## Details

\### Environmental Variable Transformation

The `transform` argument allows for different pre-processing of the
environmental raster layers to address issues like differing units and
multicollinearity.

\- "scale" (Default):\*\* This is the standard approach to handle
variables with different units (e.g., °C vs. mm). It transforms each
raster layer to have a mean of 0 and a standard deviation of 1 (Baddeley
et al., 2016). This process makes the variables equal variance. As a
result, each variable contributes equally to the analysis, ensuring that
the resulting resolutions are based on the relative distribution of data
points within each environmental dimension, not their arbitrary original
units (Beaugrand, 2024; Kléparski et al., 2021).

\- "pca": This option performs a Principal Component Analysis (PCA) on
the environmental rasters. This is a powerful technique for dealing with
multicollinearity (highly correlated variables). It transforms the
original rasters into a new set of uncorrelated layers (Principal
Components) (Qiao et al., 2016). The function then extracts the PC
scores for each occurrence point.

\- "none": This option extracts the raw environmental values from the
rasters without any transformation. This is suitable if your rasters are
already scaled or if you have a specific reason to use the raw values.

## References

Baddeley, A., Rubak, E. and Turner, R. (2016). Spatial point patterns:
methodology and applications with R. CRC press.

Beaugrand, G. (2024). An ecological niche model that considers local
relationships among variables: The Environmental String Model.
Ecosphere, 15(10), e70015.

Kléparski, L., Beaugrand, G. and Edwards, M. (2021). Plankton
biogeography in the North Atlantic Ocean and its adjacent seas: Species
assemblages and environmental signatures. Ecology and Evolution, 11(10),
5135-5149.

Qiao, H., Peterson, A. T., Campbell, L. P., Soberón, J., Ji, L. and
Escobar, L. E. (2016). NicheA: creating virtual species and ecological
niches in multivariate environmental scenarios. Ecography, 39(8),
805-813.

## Examples

``` r
# \donttest{
env_file <- system.file("extdata", "thai_env.tif", package = "bean")
occ_file <- system.file("extdata", "Rusa_unicolor.csv", package = "bean")
if (nzchar(env_file) && nzchar(occ_file) &&
    requireNamespace("terra", quietly = TRUE)) {
  env <- terra::rast(env_file)
  occ <- read.csv(occ_file)
  prepared <- prepare_bean(
    data        = occ,
    env_rasters = env,
    longitude   = "x",
    latitude    = "y",
    transform   = "scale"
  )
  head(prepared)
}
#> Scaling environmental rasters...
#> Extracting environmental data for occurrence points...
#> 5 records removed because they fell outside the raster extent or had NA environmental values.
#> Data preparation complete. Returning 1024 clean records.
#>         species        y         x      bio_1       bio_12     bio_15
#> 1 Rusa unicolor 15.37239  99.11555 -1.6909295  0.003511156 -0.2454693
#> 2 Rusa unicolor 15.41415  99.28763 -0.8711075 -0.267821213 -0.3053829
#> 3 Rusa unicolor 14.46838 101.22005 -1.3879976 -0.534812263 -0.3835742
#> 4 Rusa unicolor 15.65606  99.31600 -0.6288324 -0.224408034 -0.2390396
#> 5 Rusa unicolor 14.39543 101.41694 -2.0311866 -0.604273349 -0.5074636
#> 6 Rusa unicolor 12.72500 100.88947  1.0537073 -0.311234392 -0.6623129
#>        bio_4
#> 1 -0.2573387
#> 2 -0.1768698
#> 3 -0.2876102
#> 4 -0.0834852
#> 5 -0.1398570
#> 6 -1.0746857
# }
```
