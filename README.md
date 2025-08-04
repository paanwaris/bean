
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bean 🫛

<!-- badges: start -->

[![R-CMD-check](https://github.com/paanwaris/bean/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/paanwaris/bean/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/paanwaris/bean/graph/badge.svg)](https://app.codecov.io/gh/paanwaris/bean)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Ecological Motivation

The `bean` package provides a tool to address a fundamental challenge in
species distribution modeling (SDM, or ecological niche modeling, ENM):
**sampling bias**. Occurrence records for species are rarely collected
through a systematic, stratified process. Instead, they often cluster in
easily accessible areas (like roads and cities) or in well-studied
research sites. This spatial bias can translate into an **environmental
bias**, where the model incorrectly learns that the species is
associated with the environmental conditions of those heavily sampled
areas, rather than its true ecological requirements.

`bean` tackles this problem by thinning occurrence data in
**environmental space**. The goal is to create a more uniform
distribution of points across the species’ observed environmental niche,
reducing the influence of densely clustered records. This allows for the
construction of a more accurate **fundamental niche** volume, which can
then be projected into geographic space to create a less biased
prediction of area with environmental suitability.

The name `bean` reflects the core principle of the method: ensuring that
each “pod” (a grid cell in environmental space) contains only a
specified number of “beans” (occurrence points).

## Installation

To use the package, you first need to install it from GitHub. The
following code will check for the required `devtools` package, install
it if necessary, and then install `bean`.

``` r
# 1. Install devtools if you don't have it yet
if (!require("devtools")) {
  install.packages("devtools")
}

# 2. Install bean from GitHub
devtools::install_github("paanwaris/bean")
```

## Package Loading

To perform the necessary analyses, we need several R packages for data
manipulation, visualization, and modeling.

``` r
# Load required libraries
library(bean)
library(dplyr)
library(ggplot2)
library(GGally)
library(terra)
library(raster)
library(rgl)
library(dismo)
```

## The `bean` Protocol: A Step-by-Step Guide

### Step 1: Data Preparation and Visualization

The first step is to load your raw species occurrence data and
environmental raster layers. It’s always a good practice to visualize
the spatial distribution of your points and inspect the environmental
layers.

``` r
# Load the raw occurrence data from the package
occ_file <- system.file("extdata", "Rattus_norvegicus.csv", package = "bean")
occ_data_raw <- read.csv(occ_file)

# Display the first few rows to understand its structure
head(occ_data_raw)
#>             species          x        y
#> 1 Rattus norvegicus -118.20774 34.20412
#> 2 Rattus norvegicus -118.38869 34.07236
#> 3 Rattus norvegicus -122.25995 47.67489
#> 4 Rattus norvegicus  -71.38052 41.75420
#> 5 Rattus norvegicus  -72.36896 43.75171
#> 6 Rattus norvegicus  -81.46850 41.08183

# Visualize the spatial distribution of the occurrence points
ggplot(occ_data_raw, aes(x = x, y = y)) +
  geom_point(alpha = 0.5, color = "darkred") +
  coord_fixed() +
  labs(title = "Raw Occurrence Point Distribution") +
  theme_bw()
```

<img src="man/figures/README-setup-1-1.png" width="100%" />

``` r
# Load the environmental raster layers
PC1_file <- system.file("extdata", "PC1.tif", package = "bean")
PC2_file <- system.file("extdata", "PC2.tif", package = "bean")
PC3_file <- system.file("extdata", "PC3.tif", package = "bean")
env_pca <- terra::rast(c(PC1_file, PC2_file, PC3_file))

# Plot the environmental layers to check their extent and values
plot(env_pca, mar = c(1, 1, 2, 4))
```

<img src="man/figures/README-setup-2-1.png" width="100%" />

### Step 2: Prepare Data for Environmental Thinning

Before any analysis, the raw data must be cleaned and standardized. The
`prepare_bean()` function streamlines this process by: 1. Removing
records with missing coordinates. 2. Extracting environmental data for
each point from scaled raster layers. 3. Removing records that fall
outside the raster extent.

This ensures all subsequent functions work with a clean, complete, and
scaled dataset.

``` r
# Run the preparation function to clean and scale the data
origin_dat_prepared <- prepare_bean(
  data = occ_data_raw,
  env_rasters = env_pca,
  longitude = "x",
  latitude = "y",
  transform = "none"
)
#> Skipping raster transformation.
#> Extracting environmental data for occurrence points...
#> 992 records removed because they fell outside the raster extent or had NA environmental values.
#> Data preparation complete. Returning 2960 clean records.

# View the structure and summary of the clean, scaled data
head(origin_dat_prepared)
#>             species          x        y        PC1       PC2        PC3
#> 1 Rattus norvegicus -118.20774 34.20412  2.2750390  5.050178 -0.1533088
#> 2 Rattus norvegicus -118.38869 34.07236  3.2544551  5.137757 -0.9032499
#> 3 Rattus norvegicus -122.25995 47.67489  1.6397074  1.628879 -2.9925177
#> 5 Rattus norvegicus  -72.36896 43.75171 -2.6410787 -2.522105 -1.0693041
#> 6 Rattus norvegicus  -81.46850 41.08183  0.5351064 -1.787927 -1.1424713
#> 7 Rattus norvegicus -122.28915 37.82731  3.4546990  3.520514 -3.8269217
summary(origin_dat_prepared)
#>    species                x                 y              PC1         
#>  Length:2960        Min.   :-124.19   Min.   :25.66   Min.   :-4.0466  
#>  Class :character   1st Qu.: -98.48   1st Qu.:38.91   1st Qu.:-0.3467  
#>  Mode  :character   Median : -77.04   Median :40.73   Median : 0.9991  
#>                     Mean   : -87.85   Mean   :40.34   Mean   : 1.0781  
#>                     3rd Qu.: -73.94   3rd Qu.:42.33   3rd Qu.: 1.5478  
#>                     Max.   : -67.17   Max.   :48.92   Max.   : 9.4357  
#>       PC2               PC3         
#>  Min.   :-2.9778   Min.   :-8.4241  
#>  1st Qu.:-1.4589   1st Qu.:-1.0351  
#>  Median :-1.1918   Median :-0.6360  
#>  Mean   :-0.3783   Mean   :-1.0070  
#>  3rd Qu.:-0.2096   3rd Qu.:-0.3796  
#>  Max.   : 5.1761   Max.   : 2.5056

# Visualize the spatial distribution of the cleaned occurrence points
ggplot(origin_dat_prepared, aes(x = x, y = y)) +
  geom_point(alpha = 0.5, color = "darkred") +
  coord_fixed() +
  labs(title = "Raw Occurrence Point Distribution") +
  theme_bw()
```

<img src="man/figures/README-prepare-data-1.png" width="100%" />

### Step 3: Objective Grid Resolution using Pairwise Distances

The most critical parameter in environmental gridding is the
`grid_resolution`. Instead of guessing this value, we can derive it
objectively from the data by analyzing the **distribution of pairwise
environmental distances**.

The logic is to calculate the Euclidean distance between all pairs of
points in the (scaled) environmental space. The quantile of pairwise
distances to use for the resolution. A smaller quantile value (e.g.,
0.05-0.25) is generally recommended for gentle thinning. However, for
widespread generalists, a higher quantile (e.g., \> 0.75) may improve
model performance by more aggressively reducing large-scale bias

The `find_env_resolution()` function automates this process.

``` r
# Set a seed for reproducibility of the resampling in the correlogram
set.seed(81)  

# Let's set a low quantile value at 0.10
resolution_results <- find_env_resolution(
  data = origin_dat_prepared,
  env_vars = c("PC1", "PC2", "PC3"),
  quantile = 0.10
)
#> Calculating pairwise distances for each environmental axis...

# The function returns a suggested resolution and the full distance distribution
resolution_results
#> --- Bean Environmental Resolution Analysis ---
#> 
#> Suggested Grid Resolutions (at the 10% quantile):
#>   - PC1: 0.186294
#>   - PC2: 0.105957
#>   - PC3: 0.071493
#> 
#> To see the full distance distributions, run plot(your_results_object).

# We can also plot the distribution to visualize the analysis
# The blue line shows the distance at the chosen quantile.
plot(resolution_results)
```

<img src="man/figures/README-find-resolution-1.png" width="100%" />

``` r

# Let's use this objective resolution in the next step
grid_res <- resolution_results$suggested_resolution
```

### Step 4: Parameter Exploration with `find_optimal_cap()`

This is the most important step for ensuring a defensible thinning
strategy. Instead of guessing parameters, `find_optimal_cap()` allows
you to explore the trade-offs and make a data-driven choice.

**Key Parameters:** \* `grid_resolution`: This defines the size of the
cells in your environmental grid. The choice is ecologically
significant. A small value creates a fine grid, which is sensitive to
small environmental variations but may not thin large, dense clusters
effectively. A large value creates a coarse grid, which is better for
thinning broad-scale bias but may group distinct environmental
conditions together. \* `target_percent`: This is your goal for data
retention. A value of `0.8` means you want to keep approximately 80% of
your data. A value of `0.95` is recommended to remove 5% of the most
densely clustered points while retaining most of the data.

The function returns two key recommendations to guide your choice: 1.
`best_cap_closest`: The cap that results in a point count *numerically
closest* to your target. 2. `best_cap_above_target`: The cap that
results in a point count that is *closest to, but not below*, your
target. This is often the safer, more conservative choice if you want to
avoid losing too much data.

``` r
set.seed(81)
# Let's target retaining 95% of the data as recommended
optimal_params <- find_optimal_cap(
  data = origin_dat_prepared,
  env_vars = c("PC1", "PC2", "PC3"),
  grid_resolution = grid_res,
  target_percent = 0.95
)
#> Searching for optimal cap...
#>   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |=========                                                             |  14%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |============================                                          |  41%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |===================================                                   |  51%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |============================================                          |  64%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%

# The function automatically saves results to the output directory.
# We can also inspect the returned list object.
# Print the recommendations
optimal_params
#> --- Bean Optimization Results ---
#> 
#> Target: Retain >= 2812 occurrence points.
#> 
#> Recommendation for 'Closest to Target':
#>   - Best Cap: 83
#>   - Retained Points: 2811 (Difference of 1)
#> 
#> Recommendation for 'Closest Above Target' (Recommended for use):
#>   - Best Cap: 84
#>   - Retained Points: 2814
#> 
#> ---------------------------------

# Visualize the search process to understand the trade-offs
# The plot is also saved as a PNG in the output directory.
plot(optimal_params)
```

<img src="man/figures/README-find-and-thin-part1-1.png" width="100%" />

### Step 5: Apply Thinning

Now that we have an objective `grid_resolution` and an optimal
`max_per_cell`, we can apply the thinning. We offer two methods:
stochastic and deterministic.

#### Method A: Stochastic Thinning with `thin_env_nd`

This method randomly samples up to `max_per_cell` points from each
occupied grid cell. It’s the most common approach.

``` r
# Use the recommended cap from the previous step
chosen_cap <- optimal_params$best_cap_above_target

# Apply the stochastic thinning
thinned_stochastic <- thin_env_nd(
  data = origin_dat_prepared,
  env_vars = c("PC1", "PC2", "PC3"),
  grid_resolution = grid_res, 
  max_per_cell = chosen_cap
)

# Print the summary of the thinning results
thinned_stochastic
#> --- Bean Stochastic Thinning Results ---
#> 
#> Thinned 2960 original points to 2814 points.
#> This represents a retention of 95.1% of the data.
#> 
#> --------------------------------------
head(thinned_stochastic$thinned_data)
#>             species         x        y       PC1       PC2        PC3
#> 1 Rattus norvegicus -73.17725 43.91748 -1.687248 -2.008513 -1.2505924
#> 2 Rattus norvegicus -72.82629 43.85227 -1.715427 -2.083218 -1.1494861
#> 3 Rattus norvegicus -72.82643 43.85228 -1.715427 -2.083218 -1.1494861
#> 4 Rattus norvegicus -88.73934 42.39872 -1.752907 -2.067283  0.7560112
#> 5 Rattus norvegicus -88.21950 43.01554 -1.809311 -2.077075  0.5865901
#> 6 Rattus norvegicus -72.80315 43.49283 -1.702178 -2.170760 -1.1895545
```

#### Method B: Deterministic Thinning with `thin_env_center`

This method provides a simpler, non-random alternative. It returns a
single new point at the exact center of every occupied grid cell,
regardless of how many points were originally in it.

``` r
# Apply the deterministic thinning
thinned_deterministic <- thin_env_center(
  data = origin_dat_prepared,
  env_vars = c("PC1", "PC2", "PC3"),
  grid_resolution = grid_res
)

# Print the summary of the thinning results
thinned_deterministic
#> --- Bean Deterministic Thinning Results ---
#> 
#> Thinned 2960 original points to 865 unique grid cell centers.
#> 
#> -----------------------------------------
head(thinned_deterministic$thinned_points)
#>          PC1       PC2        PC3
#> 1  2.3286805  5.032948 -0.1787330
#> 2  3.2601526  5.138904 -0.8936651
#> 3  1.5835027  1.642330 -2.9669681
#> 4 -2.7012693 -2.489985 -1.0366515
#> 5  0.4657361 -1.748287 -1.1081447
#> 6  3.4464471  3.549553 -3.8248866
```

### Step 6: Visualize the Thinning Results

The `plot_bean()` function provides a powerful way to visualize the
effect of thinning by overlaying the thinned points on the original data
within the environmental grid.

``` r
# Visualize the stochastic thinning results
plot_stochastic <- plot_bean(
  original_data = origin_dat_prepared,
  thinned_object = thinned_stochastic,
  env_vars = c("PC1", "PC2", "PC3")
)

# Visualize the deterministic thinning results
plot_deterministic <- plot_bean(
  original_data = origin_dat_prepared,
  thinned_object = thinned_deterministic,
  env_vars = c("PC1", "PC2", "PC3")
)

# Display plots side-by-side (requires cowplot or similar package)
# cowplot::plot_grid(plot_stochastic, plot_deterministic)
plot_stochastic
```

<img src="man/figures/README-plot-thinning-results-1.png" width="100%" />

``` r
plot_deterministic
```

<img src="man/figures/README-plot-thinning-results-2.png" width="100%" />

### Step 7: Delineate and Visualize the Niche Ellipse

After thinning, we can formalize the environmental niche by fitting a
bivariate ellipse around the points. The `fit_ellipsoid()` function
delineates this boundary.

### Stochastic Thinned Ellipsoid

``` r
# Fit an ellipse that contains 95% of the thinned data
stochastic_ellipse <- fit_ellipsoid(data = thinned_stochastic$thinned_data, 
                                    env_vars = c("PC1", "PC2", "PC3"), 
                                    method = "covmat", 
                                    level = 0.95)
# The returned object contains all the details
# We can use the custom print() method for a clean summary
stochastic_ellipse
#> --- Bean Environmental Niche Ellipsoid ---
#> 
#> Method: 'covmat'.
#> Fitted in 3 dimensions to 2814 data points at a 95.00% level.
#> 2478 out of 2814 points (88.1%) fall within the ellipsoid boundary.
#> 
#> Niche Centroid:
#>        PC1        PC2        PC3 
#>  1.1090809 -0.3244557 -1.0285415

# And we can use the custom plot() method for a powerful visualization
plot(stochastic_ellipse)
```
<img src="man/figures/README-fit-ellipse-part1-1.png" width="100%" />

### Deterministic Thinned Ellipsoid

``` r
# Fit an ellipse that contains 95% of the thinned data
deterministic_ellipse <- fit_ellipsoid(data = thinned_deterministic$thinned_points,
                                       env_vars = c("PC1", "PC2", "PC3"), 
                                       method = "covmat", 
                                       level = 0.95)
# The returned object contains all the details
# We can use the custom print() method for a clean summary
deterministic_ellipse
#> --- Bean Environmental Niche Ellipsoid ---
#> 
#> Method: 'covmat'.
#> Fitted in 3 dimensions to 865 data points at a 95.00% level.
#> 808 out of 865 points (93.4%) fall within the ellipsoid boundary.
#> 
#> Niche Centroid:
#>       PC1       PC2       PC3 
#>  1.526214  0.106508 -1.265099

# And we can use the custom plot() method for a powerful visualization
plot(deterministic_ellipse)
```
<img src="man/figures/README-fit-ellipse-part2-1.png" width="100%" />

### Step 8: Evaluate Model Performance

Finally, we test whether thinning improved our model. We will build and
evaluate two sets of Maxent models—one with the original (but cleaned)
data and one with the `bean`-thinned data—and then statistically compare
their performance.

``` r
# Create background points by sampling from the study area
# Note: We use the unscaled rasters here for sampling background points.
set.seed(123)

# Create background points by sampling from the study area
background_points <- randomPoints(raster::stack(env_pca), 1000)
colnames(background_points) <- c("x", "y")
```

``` r
# --- Run Evaluation on ORIGINAL Data ---
# Note: In a real analysis, use a higher n_repeats (e.g., 50 or 100).
auc_original <- test_env_thinning(
  presence_data = origin_dat_prepared, # Use the cleaned, but unthinned data
  background_data = background_points,
  env_rasters = env_pca,
  longitude = "x",
  latitude = "y", 
  k = 5, 
  n_repeats = 20,
  maxent_args = c("linear=true", 
                  "quadratic=true", 
                  "product=false",
                  "threshold=false", 
                  "hinge=false", 
                  "doclamp=false")
)
#> Starting 20 repetitions of 5-fold cross-validation...
#>   - Repetition 1 of 20...
#>   - Repetition 2 of 20...
#>   - Repetition 3 of 20...
#>   - Repetition 4 of 20...
#>   - Repetition 5 of 20...
#>   - Repetition 6 of 20...
#>   - Repetition 7 of 20...
#>   - Repetition 8 of 20...
#>   - Repetition 9 of 20...
#>   - Repetition 10 of 20...
#>   - Repetition 11 of 20...
#>   - Repetition 12 of 20...
#>   - Repetition 13 of 20...
#>   - Repetition 14 of 20...
#>   - Repetition 15 of 20...
#>   - Repetition 16 of 20...
#>   - Repetition 17 of 20...
#>   - Repetition 18 of 20...
#>   - Repetition 19 of 20...
#>   - Repetition 20 of 20...
#> Validation complete.

auc_original
#> --- Bean Model Evaluation Results ---
#> 
#> Based on 20 repetitions of 5-fold cross-validation (100 total models).
#> 
#> Summary of AUC Scores:
#>   Mean_AUC SD_AUC Median_AUC Min_AUC Max_AUC
#> 1    0.841  0.005      0.841   0.823   0.853
#> 
#> To see the distribution of AUC scores, run plot(your_results_object).
plot(auc_original)
```

<img src="man/figures/README-model-evaluation-2-1.png" width="100%" />

``` r
# --- Run Evaluation on THINNED Data ---
auc_thinned <- test_env_thinning(
  presence_data = stochastic_ellipse$points_in_ellipse, # Use the thinned data
  background_data = background_points,
  env_rasters = env_pca,
  longitude = "x",
  latitude = "y", 
  k = 5, 
  n_repeats = 20,
  maxent_args = c("linear=true", 
                  "quadratic=true", 
                  "product=false",
                  "threshold=false", 
                  "hinge=false", 
                  "doclamp=false")
)
#> Starting 20 repetitions of 5-fold cross-validation...
#>   - Repetition 1 of 20...
#>   - Repetition 2 of 20...
#>   - Repetition 3 of 20...
#>   - Repetition 4 of 20...
#>   - Repetition 5 of 20...
#>   - Repetition 6 of 20...
#>   - Repetition 7 of 20...
#>   - Repetition 8 of 20...
#>   - Repetition 9 of 20...
#>   - Repetition 10 of 20...
#>   - Repetition 11 of 20...
#>   - Repetition 12 of 20...
#>   - Repetition 13 of 20...
#>   - Repetition 14 of 20...
#>   - Repetition 15 of 20...
#>   - Repetition 16 of 20...
#>   - Repetition 17 of 20...
#>   - Repetition 18 of 20...
#>   - Repetition 19 of 20...
#>   - Repetition 20 of 20...
#> Validation complete.

auc_thinned
#> --- Bean Model Evaluation Results ---
#> 
#> Based on 20 repetitions of 5-fold cross-validation (100 total models).
#> 
#> Summary of AUC Scores:
#>   Mean_AUC SD_AUC Median_AUC Min_AUC Max_AUC
#> 1    0.875  0.004      0.876   0.863   0.888
#> 
#> To see the distribution of AUC scores, run plot(your_results_object).
plot(auc_thinned)
```

<img src="man/figures/README-model-evaluation-3-1.png" width="100%" />

``` r
# --- Statistically Compare the Results ---
# Perform a two-sample t-test to see if the difference in AUC is significant
auc_ttest <- t.test(auc_original$all_auc_scores, auc_thinned$all_auc_scores)
auc_ttest
#> 
#>  Welch Two Sample t-test
#> 
#> data:  auc_original$all_auc_scores and auc_thinned$all_auc_scores
#> t = -52.648, df = 192.89, p-value < 2.2e-16
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.03601820 -0.03341697
#> sample estimates:
#> mean of x mean of y 
#> 0.8406491 0.8753667

# --- Visualize the Comparison ---
# Combine results into a data frame for plotting
results_df <- data.frame(
  AUC = c(auc_original$all_auc_scores, auc_thinned$all_auc_scores),
  DataType = factor(
    rep(c("Original", "Thinned"), 
        each = length(auc_original$all_auc_scores)), 
    levels = c("Original", "Thinned")
  )
)

# Create the final comparison boxplot
ggplot(results_df, aes(x = DataType, y = AUC, fill = DataType)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +
  labs(
    title = "Comparison of Model Performance (AUC)",
    subtitle = "Comparing models built with original vs. bean-thinned data",
    x = "Presence Data Type",
    y = "Area Under Curve (AUC)"
  ) +
  scale_fill_manual(values = c("Original" = "#D55E00", "Thinned" = "#0072B2")) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = 1.5, y = min(results_df$AUC) * 0.99,
           label = paste("T-test p-value =", format.pval(auc_ttest$p.value, digits = 3)),
           hjust = 0.5, vjust = 0, fontface = "italic", size = 4)
```

<img src="man/figures/README-model-evaluation-4-1.png" width="100%" />

### Step 9: Automated Calibration with `calibrate_bean`

The manual steps above are excellent for understanding the process, but
the `calibrate_bean()` function automates the entire workflow. It is the
“final boss” of the package, testing multiple combinations of thinning
parameters (`quantile`, `method`, `cap`) and identifying the single best
set based on final model performance (AUC). It also provides statistical
comparisons against baseline models.

## Generalist Model Calibration

``` r
# Create background points for model evaluation
set.seed(123)
background_df <- as.data.frame(
  dismo::randomPoints(raster::stack(env_pca), 1000)
)
colnames(background_df) <- c("x", "y")
```

``` r
set.seed(81)
# Calibrate all key parameters
# Note: In a real analysis, use higher thinning_reps and n_repeats.
generalist_calibration <- calibrate_bean(
  data = origin_dat_prepared,
  env_vars = c("PC1", "PC2", "PC3"),
  background_data = background_df,
  env_rasters = env_pca,
  longitude = "x",
  latitude = "y",
  quantile_range = seq(0.05, 0.95, 0.05), 
  method_range = c("covmat", "mve"),     
  target_percent = 0.95,
  level = 0.95,
  thinning_reps = 4, 
  k = 5,
  n_repeats = 10,      
  maxent_args = c("linear=true", 
                  "quadratic=true", 
                  "product=false",
                  "threshold=false", 
                  "hinge=false", 
                  "doclamp=false"
))
```

``` r
# Print the summary table to see the best combination of parameters
generalist_calibration
#> --- Bean Parameter Calibration Results ---
#> 
#> Search Summary (sorted by performance):
#> # A tibble: 41 × 6
#>    combination     mean_auc sd_auc p_value_vs_original significance group
#>    <chr>              <dbl>  <dbl> <chr>               <chr>        <chr>
#>  1 Original_covmat    0.88   0.004 1.3e-11             ***          a    
#>  2 q0.35_covmat       0.878  0.004 1.3e-11             ***          ah   
#>  3 q0.30_covmat       0.877  0.004 1.3e-11             ***          afh  
#>  4 q0.25_covmat       0.877  0.005 1.3e-11             ***          aefh 
#>  5 q0.15_covmat       0.877  0.005 1.3e-11             ***          aefgh
#>  6 q0.95_covmat       0.876  0.004 1.3e-11             ***          aefgh
#>  7 q0.90_covmat       0.876  0.005 1.3e-11             ***          aefgh
#>  8 q0.85_covmat       0.875  0.005 1.3e-11             ***          aefgh
#>  9 q0.10_covmat       0.875  0.005 1.3e-11             ***          efgh 
#> 10 q0.75_covmat       0.875  0.005 1.3e-11             ***          efgh 
#> 11 q0.65_covmat       0.875  0.004 1.3e-11             ***          efgh 
#> 12 q0.60_covmat       0.875  0.005 1.3e-11             ***          efgh 
#> 13 q0.05_covmat       0.875  0.004 1.3e-11             ***          efgh 
#> 14 q0.80_covmat       0.875  0.005 1.3e-11             ***          efgh 
#> 15 q0.70_covmat       0.875  0.005 1.3e-11             ***          efgh 
#> 16 q0.20_covmat       0.875  0.005 1.3e-11             ***          efg  
#> 17 q0.50_covmat       0.874  0.004 1.3e-11             ***          befg 
#> 18 q0.55_covmat       0.874  0.005 1.3e-11             ***          beg  
#> 19 q0.45_covmat       0.874  0.005 1.3e-11             ***          beg  
#> 20 q0.40_covmat       0.874  0.005 1.3e-11             ***          bg   
#> 21 q0.90_mve          0.871  0.005 1.3e-11             ***          d    
#> 22 Original_mve       0.87   0.006 1.3e-11             ***          bcd  
#> 23 q0.80_mve          0.87   0.005 1.3e-11             ***          cd   
#> 24 q0.70_mve          0.869  0.006 1.3e-11             ***          cd   
#> 25 q0.40_mve          0.867  0.005 1.3e-11             ***          co   
#> 26 q0.60_mve          0.865  0.009 1.3e-11             ***          no   
#> 27 q0.20_mve          0.865  0.007 1.3e-11             ***          no   
#> 28 q0.30_mve          0.865  0.009 1.3e-11             ***          no   
#> 29 q0.35_mve          0.864  0.008 1.3e-11             ***          jn   
#> 30 q0.55_mve          0.864  0.009 1.3e-11             ***          jn   
#> 31 q0.25_mve          0.863  0.009 1.3e-11             ***          jn   
#> 32 q0.05_mve          0.862  0.007 1.3e-11             ***          ij   
#> 33 q0.50_mve          0.86   0.009 1.3e-11             ***          ik   
#> 34 q0.75_mve          0.859  0.011 1.3e-11             ***          ik   
#> 35 q0.15_mve          0.859  0.007 1.3e-11             ***          ikl  
#> 36 q0.65_mve          0.858  0.015 1.3e-11             ***          kl   
#> 37 q0.10_mve          0.858  0.013 1.3e-11             ***          klm  
#> 38 q0.95_mve          0.857  0.009 1.3e-11             ***          klm  
#> 39 q0.45_mve          0.857  0.013 1.3e-11             ***          lm   
#> 40 q0.85_mve          0.855  0.007 1.3e-11             ***          m    
#> 41 Original           0.841  0.006 NA                  NA           p    
#> 
#> --- Best Combination ---
#> Optimal Quantile: 0.350
#> Optimal Ellipse Method: 'covmat'
#> Resulting Grid Resolution:
#>   - PC1: 0.9282
#>   - PC2: 0.5263
#>   - PC3: 0.3407
#> Resulting Thinning Cap: 254
#> ---
#> Significance stars (*) indicate p-value from a pairwise comparison against the 'Original' baseline model.
#> Signif. codes: '***' p < 0.001,  '**' p < 0.01,  '*' p < 0.05,  'ns' p >= 0.05
#> Models sharing a letter in the 'group' column are not significantly different from each other (Tukey's HSD).

# Plot the results to visualize the performance trade-offs
plot(generalist_calibration)
```

<img src="man/figures/README-calibrate-bean-4-1.png" width="100%" />

``` r

# You can now access the final, best-thinned data directly for your final model
generalist_data <- generalist_calibration$best_points_in_ellipse
head(generalist_data)
#>             species         x        y         PC1        PC2        PC3
#> 2 Rattus norvegicus -76.89530 40.27412 -0.07229089 -0.9086578 -0.3290531
#> 3 Rattus norvegicus -76.89195 40.26653 -0.07229089 -0.9086578 -0.3290531
#> 4 Rattus norvegicus -79.81736 39.52438 -0.14211887 -0.9671699 -0.4822511
#> 5 Rattus norvegicus -79.81769 39.52450 -0.14211887 -0.9671699 -0.4822511
#> 6 Rattus norvegicus -79.81724 39.52437 -0.14211887 -0.9671699 -0.4822511
#> 7 Rattus norvegicus -79.81713 39.52424 -0.14211887 -0.9671699 -0.4822511
```

## Specialist Model Calibration

``` r
specialist <- read.csv("inst/extdata/Ursus_arctos.csv")

specialist_prepared <- prepare_bean(
  data = specialist,
  env_rasters = env_pca,
  longitude = "x",
  latitude = "y",
  transform = "none"
)
#> Skipping raster transformation.
#> Extracting environmental data for occurrence points...
#> 432 records removed because they fell outside the raster extent or had NA environmental values.
#> Data preparation complete. Returning 1258 clean records.

# Visualize the spatial distribution of the specialist occurrence points
ggplot(specialist_prepared, aes(x = x, y = y)) +
  geom_point(alpha = 0.5, color = "darkred") +
  coord_fixed() +
  labs(title = "Raw Occurrence Point Distribution") +
  theme_bw()
```

<img src="man/figures/README-calibrate-bean-5-1.png" width="100%" />

``` r

# Create background points for model evaluation
set.seed(123)
background_df <- as.data.frame(
  dismo::randomPoints(raster::stack(env_pca), 1000)
)
colnames(background_df) <- c("x", "y")
```

``` r
# Calibrate all key parameters
# Note: In a real analysis, use higher thinning_reps and n_repeats.
specialist_calibration <- calibrate_bean(
  data = specialist_prepared,
  env_vars = c("PC1", "PC2", "PC3"),
  background_data = background_df,
  env_rasters = env_pca,
  longitude = "x",
  latitude = "y",
  quantile_range = seq(0.05, 0.95, 0.05), 
  method_range = c("covmat", "mve"),      
  target_percent = 0.50,
  level = 0.95,
  thinning_reps = 3, 
  k = 5,
  n_repeats = 10,      
  maxent_args = c("linear=true", 
                  "quadratic=true", 
                  "product=false",
                  "threshold=false", 
                  "hinge=false", 
                  "doclamp=false"
))
```

``` r
# Print the summary table to see the best combination of parameters
specialist_calibration
#> --- Bean Parameter Calibration Results ---
#> 
#> Search Summary (sorted by performance):
#> # A tibble: 41 × 6
#>    combination     mean_auc sd_auc p_value_vs_original significance group 
#>    <chr>              <dbl>  <dbl> <chr>               <chr>        <chr> 
#>  1 q0.95_mve          0.993  0.001 < 2e-16             ***          o     
#>  2 q0.50_mve          0.993  0.001 < 2e-16             ***          o     
#>  3 q0.95_covmat       0.993  0     < 2e-16             ***          o     
#>  4 q0.70_mve          0.993  0.001 < 2e-16             ***          o     
#>  5 q0.80_mve          0.993  0.001 < 2e-16             ***          do    
#>  6 q0.55_mve          0.993  0.001 < 2e-16             ***          cd    
#>  7 Original_covmat    0.993  0     < 2e-16             ***          abcd  
#>  8 Original_mve       0.993  0     < 2e-16             ***          abce  
#>  9 q0.70_covmat       0.993  0     < 2e-16             ***          bc    
#> 10 q0.45_covmat       0.993  0     < 2e-16             ***          abc   
#> 11 q0.45_mve          0.993  0     < 2e-16             ***          abce  
#> 12 q0.40_covmat       0.993  0     < 2e-16             ***          abceh 
#> 13 q0.50_covmat       0.993  0     < 2e-16             ***          abegh 
#> 14 q0.55_covmat       0.993  0     < 2e-16             ***          abefgh
#> 15 q0.20_covmat       0.993  0     < 2e-16             ***          abefgh
#> 16 q0.35_covmat       0.993  0     < 2e-16             ***          abefgh
#> 17 q0.25_mve          0.993  0.001 < 2e-16             ***          abefgh
#> 18 q0.40_mve          0.993  0     < 2e-16             ***          aefgh 
#> 19 q0.60_mve          0.993  0     < 2e-16             ***          aefgh 
#> 20 q0.05_covmat       0.993  0     < 2e-16             ***          efgh  
#> 21 q0.80_covmat       0.993  0     < 2e-16             ***          efghj 
#> 22 q0.20_mve          0.993  0     < 2e-16             ***          efghj 
#> 23 q0.85_covmat       0.993  0     < 2e-16             ***          efghij
#> 24 q0.30_covmat       0.993  0     < 2e-16             ***          efghij
#> 25 q0.10_mve          0.993  0     < 2e-16             ***          efghij
#> 26 q0.65_mve          0.993  0.001 < 2e-16             ***          efghij
#> 27 q0.25_covmat       0.993  0     < 2e-16             ***          fghijl
#> 28 q0.05_mve          0.993  0     < 2e-16             ***          fgijkl
#> 29 q0.10_covmat       0.993  0     < 2e-16             ***          fijkl 
#> 30 q0.75_mve          0.992  0     < 2e-16             ***          ijklm 
#> 31 q0.15_covmat       0.992  0     < 2e-16             ***          iklm  
#> 32 q0.60_covmat       0.992  0     < 2e-16             ***          iklm  
#> 33 q0.75_covmat       0.992  0     < 2e-16             ***          klm   
#> 34 q0.15_mve          0.992  0.001 < 2e-16             ***          km    
#> 35 q0.65_covmat       0.992  0     < 2e-16             ***          km    
#> 36 q0.35_mve          0.992  0     < 2e-16             ***          km    
#> 37 q0.90_mve          0.992  0     < 2e-16             ***          km    
#> 38 q0.30_mve          0.992  0     < 2e-16             ***          mn    
#> 39 q0.85_mve          0.992  0.001 < 2e-16             ***          np    
#> 40 q0.90_covmat       0.992  0     1.3e-07             ***          p     
#> 41 Original           0.992  0.001 NA                  NA           q     
#> 
#> --- Best Combination ---
#> Optimal Quantile: 0.950
#> Optimal Ellipse Method: 'mve'
#> Resulting Grid Resolution:
#>   - PC1: 1.6098
#>   - PC2: 0.8555
#>   - PC3: 1.0970
#> Resulting Thinning Cap: 112
#> ---
#> Significance stars (*) indicate p-value from a pairwise comparison against the 'Original' baseline model.
#> Signif. codes: '***' p < 0.001,  '**' p < 0.01,  '*' p < 0.05,  'ns' p >= 0.05
#> Models sharing a letter in the 'group' column are not significantly different from each other (Tukey's HSD).

# Plot the results to visualize the performance trade-offs
plot(specialist_calibration)
```

<img src="man/figures/README-calibrate-bean-8-1.png" width="100%" />

``` r

# You can now access the final, best-thinned data directly for your final model
specialist_data <- specialist_calibration$best_points_in_ellipse
head(specialist_data)
#>         species         x        y       PC1         PC2        PC3
#> 17 Ursus arctos -111.5523 45.22335 -3.966187 -0.06275072 -1.0688767
#> 18 Ursus arctos -111.5458 44.21059 -4.672956 -0.16924854 -0.7717657
#> 19 Ursus arctos -113.4175 47.12403 -3.867981 -0.47411591 -1.0366868
#> 20 Ursus arctos -110.7356 45.57133 -3.903330 -0.14857748 -0.7712331
#> 21 Ursus arctos -112.5559 48.31898 -4.058914 -0.29275873 -1.0248948
#> 22 Ursus arctos -113.8682 48.32234 -4.202965 -0.80534685 -1.0591527
```

### The End ❤️
