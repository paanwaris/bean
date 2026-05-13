# Niche modeling

Load the package

``` r

library(bean)
#> Warning in rgl.init(initValue, onlyNULL): RGL: unable to open X11 display
#> Warning: 'rgl.init' failed, will use the null device.
#> See '?rgl.useNULL' for ways to avoid this warning.
library(terra)
#> terra 1.9.27
library(rgl)
library(ggplot2)
```

## Step 5: Visualize the Thinning Results

The
[`plot_bean()`](https://paanwaris.github.io/bean/reference/plot_bean.md)
function provides a powerful way to visualize the effect of thinning by
overlaying the thinned points on the original data within the
environmental grid.

``` r

data(occ_data_raw, package = "bean")
data(origin_dat_prepared, package = "bean")
data(thinned_stochastic, package = "bean")
data(thinned_deterministic, package = "bean")

# Visualize the stochastic thinning results
plot_bean(
  original_data = origin_dat_prepared,
  thinned_object = thinned_stochastic,
  env_vars = c("bio_1","bio_4", "bio_12", "bio_15")
)
```

![](niche-modeling_files/figure-html/plot-thinning-results-1.png)

``` r


# Visualize the deterministic thinning results
plot_bean(
  original_data = origin_dat_prepared,
  thinned_object = thinned_deterministic,
  env_vars = c("bio_1","bio_4", "bio_12", "bio_15")
)
```

![](niche-modeling_files/figure-html/plot-thinning-results-2.png)

``` r

# Visualize the spatial distribution of the occurrence and thinned points

# Load the environmental raster layers
thai_env_file <- system.file("extdata", "thai_env.tif", package = "bean")
env <- terra::rast(c(thai_env_file))

# Combine the data and add a label for the partition
plot_data <- dplyr::bind_rows(
  occ_data_raw %>% dplyr::mutate(Data_Type = "Original (Raw)"),
  thinned_stochastic$thinned_data %>% dplyr::mutate(Data_Type = "Thinned (Stochastic)")
)

# Lock the order so "Original" is always on the left partition
plot_data$Data_Type <- factor(plot_data$Data_Type, levels = c("Original (Raw)", "Thinned (Stochastic)"))

# Plot with a partition (facet_wrap)
ggplot(plot_data, aes(x = x, y = y)) +
  geom_raster(data = as.data.frame(env[[1]], xy = TRUE), aes(x = x, y = y), fill = "gray80") +
  geom_point(alpha = 0.5, color = "darkred") +
  coord_fixed() +
  facet_wrap(~Data_Type) + # <--- This creates the partition line!
  labs(title = "Spatial Point Distribution",
       x = "Longitude", y = "Latitude") +
  theme_classic() +
  theme(strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold"))
```

![](niche-modeling_files/figure-html/unnamed-chunk-3-1.png)

## Step 6: Delineate and Visualize the Niche Ellipse

After thinning, we can formalize the environmental niche by fitting a
bivariate ellipse around the points. The
[`fit_ellipsoid()`](https://paanwaris.github.io/bean/reference/fit_ellipsoid.md)
function delineates this boundary.

### Original Ellipsoid

``` r

# Fit an ellipse that contains 95% of the original data
origin_ellipse <- fit_ellipsoid(data = origin_dat_prepared, 
                                env_vars = c("bio_1","bio_4", "bio_12", "bio_15"), 
                                method = "covmat", 
                                level = 0.95)
# The returned object contains all the details
origin_ellipse
#> --- Bean Environmental Niche Ellipsoid ---
#> 
#> Method: 'covmat'.
#> Fitted in 4 dimensions to 1024 data points at a 95.00% level.
#> 947 out of 1024 points (92.5%) fall within the ellipsoid boundary.
#> 
#> Niche Centroid:
#>      bio_1      bio_4     bio_12     bio_15 
#>   24.47107  179.68260 1191.05273   76.98212

# And we can use the custom plot() method for a powerful visualization
plot(origin_ellipse, dims = c(1, 2))
```

![3D Stochastic
Ellipsoid](niche-modeling_files/figure-html/fit-ellipse-part1-1.png)

3D Stochastic Ellipsoid

``` r

# For interactive 3D: 
plot(origin_ellipse, dims = c(1, 2, 3))
```

### Stochastic Thinned Ellipsoid

``` r

# Fit an ellipse that contains 95% of the thinned data
stochastic_ellipse <- fit_ellipsoid(data = thinned_stochastic$thinned_data, 
                                    env_vars = c("bio_1","bio_4", "bio_12", "bio_15"), 
                                    method = "covmat", 
                                    level = 0.95)
stochastic_ellipse
#> --- Bean Environmental Niche Ellipsoid ---
#> 
#> Method: 'covmat'.
#> Fitted in 4 dimensions to 78 data points at a 95.00% level.
#> 71 out of 78 points (91.0%) fall within the ellipsoid boundary.
#> 
#> Niche Centroid:
#>      bio_1      bio_4     bio_12     bio_15 
#>   25.63890  167.99191 1323.07692   76.97539

plot(stochastic_ellipse, dims = c(1, 2))
```

![3D Stochastic
Ellipsoid](niche-modeling_files/figure-html/fit-ellipse-part2-1.png)

3D Stochastic Ellipsoid

``` r

# For interactive 3D: 
plot(stochastic_ellipse, dims = c(1, 2, 3))
```

### Deterministic Thinned Ellipsoid

``` r

# Fit an ellipse that contains 95% of the thinned data
deterministic_ellipse <- fit_ellipsoid(data = thinned_deterministic$thinned_points,
                                       env_vars = c("bio_1","bio_4", "bio_12", "bio_15"), 
                                       method = "covmat", 
                                       level = 0.95)
deterministic_ellipse
#> --- Bean Environmental Niche Ellipsoid ---
#> 
#> Method: 'covmat'.
#> Fitted in 4 dimensions to 78 data points at a 95.00% level.
#> 72 out of 78 points (92.3%) fall within the ellipsoid boundary.
#> 
#> Niche Centroid:
#>      bio_1      bio_4     bio_12     bio_15 
#>   25.66026  168.01923 1323.32692   76.98718

plot(deterministic_ellipse, dims = c(1, 2))
```

![3D Deterministic
Ellipsoid](niche-modeling_files/figure-html/fit-ellipse-part3-1.png)

3D Deterministic Ellipsoid

``` r

# For interactive 3D: 
plot(deterministic_ellipse, dims = c(1, 2, 3))
```

## Step 7: Predict Niche Suitability

Finally, we project the learned fundamental niche back into geographic
space. The `predict` method calculates the continuous Mahalanobis
distance and suitability scores across the landscape.

``` r

# Predict using the unthinned ellipsoid
origin_pred <- predict(object = origin_ellipse,
                       newdata = env,
                       include_suitability = TRUE,
                       suitability_truncated = TRUE,
                       include_mahalanobis = TRUE,
                       mahalanobis_truncated = TRUE)
plot(origin_pred)
```

![](niche-modeling_files/figure-html/unnamed-chunk-4-1.png)

``` r

# Predict using the stochastic thinned ellipsoid
stochastic_pred <- predict(object = stochastic_ellipse,
                           newdata = env,
                           include_suitability = TRUE,
                           suitability_truncated = TRUE,
                           include_mahalanobis = TRUE,
                           mahalanobis_truncated = TRUE)
plot(stochastic_pred)
```

![](niche-modeling_files/figure-html/unnamed-chunk-5-1.png)

``` r

# Predict using the deterministic thinned ellipsoid
deterministic_pred <- predict(object = deterministic_ellipse,
                              newdata = env,
                              include_suitability = TRUE,
                              suitability_truncated = TRUE,
                              include_mahalanobis = TRUE,
                              mahalanobis_truncated = TRUE)
plot(deterministic_pred)
```

![](niche-modeling_files/figure-html/unnamed-chunk-6-1.png)
