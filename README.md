
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
library(terra)
library(raster)
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
occ_file <- system.file("extdata", "Peromyscus_maniculatus_original.csv", package = "bean")
occ_data_raw <- read.csv(occ_file)

# Display the first few rows to understand its structure
head(occ_data_raw)
#>                  species          x        y
#> 1 Peromyscus maniculatus -119.47519 47.37757
#> 2 Peromyscus maniculatus -119.51685 34.42286
#> 3 Peromyscus maniculatus  -77.40364 39.08822
#> 4 Peromyscus maniculatus -122.59932 45.58640
#> 5 Peromyscus maniculatus -111.85233 34.83941
#> 6 Peromyscus maniculatus -117.22582 33.38149

# Visualize the spatial distribution of the occurrence points
ggplot(occ_data_raw, aes(x = x, y = y)) +
  geom_point(alpha = 0.5, color = "darkred") +
  labs(title = "Raw Occurrence Point Distribution") +
  theme_bw()
```

<img src="man/figures/README-setup-1.png" width="100%" />

``` r

# Load the environmental raster layers
bio1_file <- system.file("extdata", "BIO1.tif", package = "bean")
bio12_file <- system.file("extdata", "BIO12.tif", package = "bean")
env_rasters <- terra::rast(c(bio1_file, bio12_file))

# Plot the environmental layers to check their extent and values
plot(env_rasters, mar = c(1, 1, 2, 4))
```

<img src="man/figures/README-setup-2.png" width="100%" />

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
  env_rasters = env_rasters,
  longitude = "x",
  latitude = "y",
  scale = TRUE
)
#> Scaling environmental rasters...
#> Extracting environmental data for occurrence points...
#> Data preparation complete. Returning 1588 clean records.

# View the structure and summary of the clean, scaled data
head(origin_dat_prepared)
#>                  species          x        y        BIO1      BIO12
#> 1 Peromyscus maniculatus -119.47519 47.37757 -0.29112739 -0.9497783
#> 2 Peromyscus maniculatus -119.51685 34.42286  1.69266992 -0.4476771
#> 3 Peromyscus maniculatus  -77.40364 39.08822  0.14013289  0.6049739
#> 4 Peromyscus maniculatus -122.59932 45.58640  0.05388084 -0.2582880
#> 5 Peromyscus maniculatus -111.85233 34.83941  0.33420002 -0.8704992
#> 6 Peromyscus maniculatus -117.22582 33.38149  1.97298911 -0.1173473
summary(origin_dat_prepared)
#>    species                x                 y              BIO1         
#>  Length:1588        Min.   :-124.35   Min.   :30.05   Min.   :-2.77087  
#>  Class :character   1st Qu.:-122.14   1st Qu.:38.46   1st Qu.:-0.74395  
#>  Mode  :character   Median :-113.85   Median :41.84   Median :-0.09706  
#>                     Mean   :-105.49   Mean   :41.49   Mean   :-0.12503  
#>                     3rd Qu.: -87.60   3rd Qu.:44.92   3rd Qu.: 0.44202  
#>                     Max.   : -67.16   Max.   :48.96   Max.   : 2.36112  
#>      BIO12         
#>  Min.   :-1.38581  
#>  1st Qu.:-0.57210  
#>  Median :-0.20984  
#>  Mean   :-0.23528  
#>  3rd Qu.: 0.08195  
#>  Max.   : 2.14211
```

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

# Let's set a higher quantile value at 0.90
resolution_results <- find_env_resolution(
  data = origin_dat_prepared,
  env_vars = c("BIO1", "BIO12"),
  quantile = 0.90
)
#> Calculating pairwise distances for each environmental axis...

# The function returns a suggested resolution and the full distance distribution
resolution_results
#> --- Bean Environmental Resolution Analysis ---
#> 
#> Suggested Grid Resolutions (at the 90% quantile):
#>   - BIO1: 2.220990
#>   - BIO12: 1.277275
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
# Let's target retaining 95% of the data as recommended
optimal_params <- find_optimal_cap(
  data = origin_dat_prepared,
  env_vars = c("BIO1", "BIO12"),
  grid_resolution = grid_res,
  target_percent = 0.95
)
#> Searching for optimal cap...
#>   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |=========                                                             |  14%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |=======================                                               |  34%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  39%  |                                                                              |============================                                          |  40%  |                                                                              |============================                                          |  41%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |===================================                                   |  51%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |=====================================                                 |  54%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |==========================================                            |  61%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |========================================================              |  81%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |=================================================================     |  94%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%

# The function automatically saves results to the output directory.
# We can also inspect the returned list object.
# Print the recommendations
optimal_params
#> --- Bean Optimization Results ---
#> 
#> Target: Retain >= 1508 occurrence points.
#> 
#> Recommendation for 'Closest to Target':
#>   - Best Cap: 525
#>   - Retained Points: 1508 (Difference of 0)
#> 
#> Recommendation for 'Closest Above Target' (Recommended for use):
#>   - Best Cap: 525
#>   - Retained Points: 1508
#> 
#> ---------------------------------

# Visualize the search process to understand the trade-offs
# The plot is also saved as a PNG in the output directory.
plot(optimal_params)
```

<img src="man/figures/README-find-and-thin-part1-1.png" width="100%" />

``` r
#The plot and the output list show that to get closest to our target of 80%.
```

### Step 5: Apply Thinning

Now that we have an objective `grid_resolution` and an optimal
`max_per_cell`, we can apply the thinning. We offer two methods:
stochastic and deterministic.

#### Method A: Stochastic Thinning with `thin_env_density`

This method randomly samples up to `max_per_cell` points from each
occupied grid cell. It’s the most common approach.

``` r
# Use the recommended cap from the previous step
chosen_cap <- optimal_params$best_cap_above_target

# Apply the stochastic thinning
thinned_stochastic <- thin_env_density(
  data = origin_dat_prepared,
  env_vars = c("BIO1", "BIO12"),
  grid_resolution = grid_res, 
  max_per_cell = chosen_cap
)

# Print the summary of the thinning results
thinned_stochastic
#> --- Bean Stochastic Thinning Results ---
#> 
#> Thinned 1588 original points to 1508 points.
#> This represents a retention of 95.0% of the data.
#> 
#> --------------------------------------
head(thinned_stochastic$thinned_data)
#>                  species          x        y       BIO1       BIO12
#> 1 Peromyscus maniculatus -118.60044 36.79720 -1.0027069 -0.99822671
#> 2 Peromyscus maniculatus -117.01755 46.71109 -0.3558164 -0.73396288
#> 3 Peromyscus maniculatus -123.09918 45.54475 -0.2911274 -0.05128132
#> 4 Peromyscus maniculatus  -73.19649 44.50337 -0.8086397 -0.02925933
#> 5 Peromyscus maniculatus  -68.40616 46.71109 -1.7142863 -0.48731664
#> 6 Peromyscus maniculatus -123.47407 47.75246 -0.5067575 -0.26269238
```

#### Method B: Deterministic Thinning with `thin_env_center`

This method provides a simpler, non-random alternative. It returns a
single new point at the exact center of every occupied grid cell,
regardless of how many points were originally in it.

``` r
# Apply the deterministic thinning
thinned_deterministic <- thin_env_center(
  data = origin_dat_prepared,
  env_vars = c("BIO1", "BIO12"),
  grid_resolution = grid_res
)

# Print the summary of the thinning results
thinned_deterministic
#> --- Bean Deterministic Thinning Results ---
#> 
#> Thinned 1588 original points to 10 unique grid cell centers.
#> 
#> -----------------------------------------
head(thinned_deterministic$thinned_points)
#>        BIO1      BIO12
#> 1 -1.110495 -0.6386376
#> 2  1.110495 -0.6386376
#> 3  1.110495  0.6386376
#> 4 -1.110495  0.6386376
#> 5 -3.331486 -0.6386376
#> 6  1.110495  1.9159128
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
  grid_resolution = grid_res,
  env_vars = c("BIO1", "BIO12")
)

# Visualize the deterministic thinning results
plot_deterministic <- plot_bean(
  original_data = origin_dat_prepared,
  thinned_object = thinned_deterministic,
  grid_resolution = grid_res,
  env_vars = c("BIO1", "BIO12")
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
                                    env_vars = c("BIO1", "BIO12"), 
                                    method = "covmat", 
                                    level = 0.95)
# The returned object contains all the details
# We can use the custom print() method for a clean summary
stochastic_ellipse
#> --- Bean Environmental Niche Ellipse ---
#> 
#> Method: 'covmat'.
#> Fitted to 1508 data points at a 95.00% level.
#> 1440 out of 1508 points (95.5%) fall within the ellipse boundary.
#> 
#> Niche Centroid:
#>        BIO1       BIO12 
#> -0.08692221 -0.21454192

# And we can use the custom plot() method for a powerful visualization
plot(stochastic_ellipse)
```

<img src="man/figures/README-fit-ellipse-part1-1.png" width="100%" />

### Deterministic Thinned Ellipsoid

``` r
# Fit an ellipse that contains 95% of the thinned data
deterministic_ellipse <- fit_ellipsoid(data = thinned_deterministic$thinned_points,
                                       env_vars = c("BIO1", "BIO12"), 
                                       method = "covmat", 
                                       level = 0.95)
# The returned object contains all the details
# We can use the custom print() method for a clean summary
deterministic_ellipse
#> --- Bean Environmental Niche Ellipse ---
#> 
#> Method: 'covmat'.
#> Fitted to 10 data points at a 95.00% level.
#> 10 out of 10 points (100.0%) fall within the ellipse boundary.
#> 
#> Niche Centroid:
#>       BIO1      BIO12 
#> -0.2220990 -0.5109101

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
set.seed(81)

# Create background points by sampling from the study area
background_points <- randomPoints(scale(raster::stack(env_rasters)), 1000)
colnames(background_points) <- c("x", "y")
```

``` r
# --- Run Evaluation on ORIGINAL Data ---
# Note: In a real analysis, use a higher n_repeats (e.g., 50 or 100).
auc_original <- test_env_thinning(
  presence_data = origin_dat_prepared, # Use the cleaned, but unthinned data
  background_data = background_points,
  env_rasters = scale(env_rasters),
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
#> 1    0.713  0.011      0.713   0.687   0.739
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
  env_rasters = scale(env_rasters),
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
#> 1    0.751  0.011       0.75   0.729   0.783
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
#> t = -24.682, df = 197.78, p-value < 2.2e-16
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.04124203 -0.03513939
#> sample estimates:
#> mean of x mean of y 
#> 0.7128503 0.7510410

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
  geom_boxplot(alpha = 0.7, width=0.5) +
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

### Step 7: Automated Calibration with `calibrate_bean`

The manual steps above are excellent for understanding the process, but
the `calibrate_bean()` function automates the entire workflow. It is the
“final boss” of the package, testing multiple combinations of thinning
parameters (`quantile`, `method`, `cap`) and identifying the single best
set based on final model performance (AUC). It also provides statistical
comparisons against baseline models.

```{r}
# Create background points for model evaluation
set.seed(81)
background_df <- as.data.frame(
  dismo::randomPoints(raster::stack(env_rasters), 1000)
)
colnames(background_df) <- c("x", "y")

# Calibrate all key parameters
# Note: In a real analysis, use higher thinning_reps and n_repeats.
# These are set low so the example runs quickly.
final_calibration <- calibrate_bean(
  data = origin_dat_prepared,
  env_vars = c("BIO1", "BIO12"),
  background_data = background_df,
  env_rasters = scale(env_rasters),
  longitude = "x",
  latitude = "y",
  quantile_range = seq(0.05, 0.95, 0.05), # Test three quantiles
  method_range = c("covmat", "mve"),      # Test both ellipsoid methods
  target_percent = 0.95,
  level = 0.95,
  thinning_reps = 3, # Use a small number for the example
  k = 5,
  n_repeats = 20,      # Use minimal settings for a runnable example
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
final_calibration
#> --- Bean Parameter Calibration Results ---
#> 
#> Search Summary (sorted by performance):
#> # A tibble: 41 × 6
#>    combination     mean_auc sd_auc p_value_vs_original significance group
#>    <chr>              <dbl>  <dbl> <chr>               <chr>        <chr>
#>  1 q0.95_covmat       0.748  0.011 < 2e-16             ***          c    
#>  2 q0.90_covmat       0.747  0.011 < 2e-16             ***          c    
#>  3 q0.95_mve          0.746  0.011 < 2e-16             ***          bc   
#>  4 Original_covmat    0.745  0.01  < 2e-16             ***          abc  
#>  5 q0.85_covmat       0.743  0.012 < 2e-16             ***          ab   
#>  6 Original_mve       0.741  0.01  < 2e-16             ***          ad   
#>  7 q0.80_covmat       0.737  0.012 < 2e-16             ***          df   
#>  8 q0.75_covmat       0.737  0.011 < 2e-16             ***          def  
#>  9 q0.05_covmat       0.737  0.012 < 2e-16             ***          def  
#> 10 q0.60_covmat       0.736  0.011 < 2e-16             ***          def  
#> 11 q0.55_covmat       0.736  0.011 < 2e-16             ***          defm 
#> 12 q0.90_mve          0.736  0.012 < 2e-16             ***          efm  
#> 13 q0.50_covmat       0.735  0.011 < 2e-16             ***          eflm 
#> 14 q0.70_covmat       0.734  0.011 < 2e-16             ***          eflm 
#> 15 q0.10_covmat       0.734  0.012 < 2e-16             ***          efklm
#> 16 q0.45_covmat       0.733  0.011 < 2e-16             ***          eklm 
#> 17 q0.65_covmat       0.733  0.012 < 2e-16             ***          eklmp
#> 18 q0.30_covmat       0.733  0.01  < 2e-16             ***          eklmp
#> 19 q0.15_covmat       0.733  0.011 < 2e-16             ***          klmp 
#> 20 q0.25_covmat       0.732  0.011 < 2e-16             ***          klmp 
#> 21 q0.35_covmat       0.732  0.012 < 2e-16             ***          klpr 
#> 22 q0.20_covmat       0.731  0.01  < 2e-16             ***          gkpr 
#> 23 q0.40_covmat       0.731  0.011 < 2e-16             ***          gkpr 
#> 24 q0.75_mve          0.731  0.011 < 2e-16             ***          gkpr 
#> 25 q0.85_mve          0.73   0.011 < 2e-16             ***          gjpr 
#> 26 q0.25_mve          0.728  0.011 < 2e-16             ***          gijr 
#> 27 q0.05_mve          0.728  0.011 < 2e-16             ***          ghij 
#> 28 q0.80_mve          0.726  0.011 < 2e-16             ***          hijs 
#> 29 q0.60_mve          0.726  0.01  < 2e-16             ***          hiqs 
#> 30 q0.20_mve          0.725  0.011 < 2e-16             ***          hnqs 
#> 31 q0.55_mve          0.725  0.01  < 2e-16             ***          hnqs 
#> 32 q0.30_mve          0.724  0.011 < 2e-16             ***          noqs 
#> 33 q0.15_mve          0.723  0.011 < 2e-16             ***          noq  
#> 34 q0.50_mve          0.723  0.01  < 2e-16             ***          noq  
#> 35 q0.10_mve          0.722  0.01  < 2e-16             ***          no   
#> 36 q0.35_mve          0.722  0.011 < 2e-16             ***          no   
#> 37 q0.40_mve          0.722  0.011 < 2e-16             ***          no   
#> 38 q0.65_mve          0.722  0.011 4.0e-10             ***          no   
#> 39 q0.70_mve          0.722  0.011 1.7e-09             ***          no   
#> 40 q0.45_mve          0.721  0.011 7.4e-08             ***          o    
#> 41 Original           0.713  0.011 NA                  NA           t    
#> 
#> --- Best Combination ---
#> Optimal Quantile: 0.950
#> Optimal Ellipse Method: 'covmat'
#> Resulting Grid Resolution:
#>   - BIO1: 2.6091
#>   - BIO12: 1.5283
#> Resulting Thinning Cap: 559
#> ---
#> Significance stars (*) indicate p-value from a pairwise comparison against the 'Original' baseline model.
#> Signif. codes: '***' p < 0.001,  '**' p < 0.01,  '*' p < 0.05,  'ns' p >= 0.05
#> Models sharing a letter in the 'group' column are not significantly different from each other (Tukey's HSD).

# Plot the results to visualize the performance trade-offs
plot(final_calibration)
```

<img src="man/figures/README-calibrate-bean-2-1.png" width="100%" />

``` r

# You can now access the final, best-thinned data directly for your final model
final_data <- final_calibration$best_points_in_ellipse
head(final_data)
#>                  species          x        y       BIO1       BIO12
#> 1 Peromyscus maniculatus -120.51657 44.50337 -0.1617493 -0.83085962
#> 2 Peromyscus maniculatus  -87.44250 46.62778 -1.1105219 -0.31114075
#> 3 Peromyscus maniculatus  -93.35751 43.75358 -0.7439507 -0.03366373
#> 4 Peromyscus maniculatus  -90.40000 47.75246 -1.6711603 -0.61944855
#> 5 Peromyscus maniculatus -117.89230 46.04461 -0.1833123 -0.61063976
#> 7 Peromyscus maniculatus -117.35079 47.66915 -0.6145726 -0.87049919
```

### The End ❤️
