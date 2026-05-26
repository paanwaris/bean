# 1. Data preparation

[`prepare_bean()`](https://paanwaris.github.io/bean/reference/prepare_bean.md)
cleans raw occurrence records and extracts environmental values for each
point:

1.  drops records with missing coordinates;
2.  (optionally) standardises the raster layers (`"scale"`) or projects
    them onto principal components (`"pca"`);
3.  extracts environmental values for each occurrence;
4.  drops records that fall outside the raster extent.

``` r

library(bean)
library(terra)
#> terra 1.9.27

occ_file <- system.file("extdata", "Rusa_unicolor.csv", package = "bean")
env_file <- system.file("extdata", "thai_env.tif",     package = "bean")

occ_data_raw <- read.csv(occ_file)
env <- terra::rast(env_file)
head(occ_data_raw)
#>         species        y         x
#> 1 Rusa unicolor 15.37239  99.11555
#> 2 Rusa unicolor 15.41415  99.28763
#> 3 Rusa unicolor 14.46838 101.22005
#> 4 Rusa unicolor 15.65606  99.31600
#> 5 Rusa unicolor 14.39543 101.41694
#> 6 Rusa unicolor 12.72500 100.88947
```

Inspect spatial distribution (only if `ggplot2` is available):

``` r

library(ggplot2)
env_df <- as.data.frame(env[[1]], xy = TRUE)
ggplot(occ_data_raw, aes(x, y)) +
  geom_raster(data = env_df, aes(x, y), fill = "grey80") +
  geom_point(alpha = 0.5, colour = "darkred") +
  coord_fixed() +
  theme_classic()
```

![](data-preparation_files/figure-html/unnamed-chunk-3-1.png)

Run the preparation step:

``` r

prepared <- prepare_bean(
  data        = occ_data_raw,
  env_rasters = env,
  longitude   = "x",
  latitude    = "y",
  transform   = "scale"
)
#> Scaling environmental rasters...
#> Extracting environmental data for occurrence points...
#> 5 records removed because they fell outside the raster extent or had NA environmental values.
#> Data preparation complete. Returning 1024 clean records.
head(prepared)
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
```

For users without `terra`, the package ships a prepared copy of the same
data:

``` r

data(origin_dat_prepared, package = "bean")
head(origin_dat_prepared)
#>         species        y         x    bio_1 bio_12   bio_15    bio_4
#> 1 Rusa unicolor 15.37239  99.11555 23.66463   1414 78.85445 175.0943
#> 2 Rusa unicolor 15.41415  99.28763 24.87193   1289 78.20957 180.2106
#> 3 Rusa unicolor 14.46838 101.22005 24.11074   1166 77.36796 173.1696
#> 4 Rusa unicolor 15.65606  99.31600 25.22871   1309 78.92366 186.1482
#> 5 Rusa unicolor 14.39543 101.41694 23.16356   1134 76.03447 182.5639
#> 6 Rusa unicolor 12.72500 100.88947 27.70646   1269 74.36774 123.1260
```
