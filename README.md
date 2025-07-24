
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
species distribution modeling (SDM) and ecological niche modeling (ENM):
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
plot(env_rasters)
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
  latitude = "y"
)
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
points in the (scaled) environmental space. A small quantile of this
distribution (e.g., the 10th percentile) represents a typical distance
between “close” points. Using this value as the grid resolution is
statistically justified because it adapts the cell size to the inherent
scale of clustering within the dataset.

The `find_env_resolution()` function automates this process.

``` r
# Set a seed for reproducibility of the resampling in the correlogram
set.seed(81)  

# A smaller value (e.g., 0.05 or 0.1) is recommended to retain most of the original points and it represents the spacing between closely clustered points.
resolution_results <- find_env_resolution(
  data = origin_dat_prepared,
  env_vars = c("BIO1", "BIO12"),
  quantile = 0.1
)
#> Calculating pairwise distances for each environmental axis...

# The function returns a suggested resolution and the full distance distribution
resolution_results
#> --- Bean Environmental Resolution Analysis ---
#> 
#> Suggested Grid Resolutions (at the 10% quantile):
#>   - BIO1: 0.150941
#>   - BIO12: 0.079279
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
#>   |                                                                              |                                                                      |   0%  |                                                                              |==                                                                    |   2%  |                                                                              |===                                                                   |   5%  |                                                                              |=====                                                                 |   7%  |                                                                              |=======                                                               |  10%  |                                                                              |=========                                                             |  12%  |                                                                              |==========                                                            |  15%  |                                                                              |============                                                          |  17%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  22%  |                                                                              |=================                                                     |  24%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  29%  |                                                                              |======================                                                |  32%  |                                                                              |========================                                              |  34%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  39%  |                                                                              |=============================                                         |  41%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  46%  |                                                                              |==================================                                    |  49%  |                                                                              |====================================                                  |  51%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  56%  |                                                                              |=========================================                             |  59%  |                                                                              |===========================================                           |  61%  |                                                                              |============================================                          |  63%  |                                                                              |==============================================                        |  66%  |                                                                              |================================================                      |  68%  |                                                                              |==================================================                    |  71%  |                                                                              |===================================================                   |  73%  |                                                                              |=====================================================                 |  76%  |                                                                              |=======================================================               |  78%  |                                                                              |========================================================              |  80%  |                                                                              |==========================================================            |  83%  |                                                                              |============================================================          |  85%  |                                                                              |=============================================================         |  88%  |                                                                              |===============================================================       |  90%  |                                                                              |=================================================================     |  93%  |                                                                              |===================================================================   |  95%  |                                                                              |====================================================================  |  98%  |                                                                              |======================================================================| 100%

# The function automatically saves results to the output directory.
# We can also inspect the returned list object.
# Print the recommendations
optimal_params
#> --- Bean Optimization Results ---
#> 
#> Target: Retain >= 1508 occurrence points.
#> 
#> Recommendation for 'Closest to Target':
#>   - Best Cap: 20
#>   - Retained Points: 1508 (Difference of 0)
#> 
#> Recommendation for 'Closest Above Target' (Recommended for use):
#>   - Best Cap: 20
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
#>                  species         x        y      BIO1      BIO12
#> 1 Peromyscus maniculatus -120.6832 47.29426 -1.455530 -0.9057344
#> 2 Peromyscus maniculatus -114.1017 46.62778 -1.477093 -1.0114399
#> 3 Peromyscus maniculatus -114.9348 47.46088 -1.433967 -0.9938223
#> 4 Peromyscus maniculatus -114.1434 46.71109 -1.412404 -1.0158443
#> 5 Peromyscus maniculatus -113.4352 46.91936 -1.498656 -1.0202487
#> 6 Peromyscus maniculatus -112.0189 46.58612 -1.369278 -1.0819103
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
#> Thinned 1588 original points to 371 unique grid cell centers.
#> 
#> -----------------------------------------
head(thinned_deterministic$thinned_points)
#>          BIO1      BIO12
#> 1 -0.22641165 -0.9117102
#> 2  1.73582265 -0.4360353
#> 3  0.07547055  0.5945936
#> 4  0.07547055 -0.2774770
#> 5  0.37735275 -0.8324311
#> 6  2.03770485 -0.1189187
```

### Step 6: Visualize the Thinning Results

To understand the effect of thinning, we can visualize the original and
thinned points on the environmental grid.

``` r
# --- Calculate Grid Line Positions for Plotting ---
# For the x-axis (BIO1)
x_range <- range(origin_dat_prepared$BIO1, na.rm = TRUE)
x_breaks <- seq(
  from = floor(x_range[1] / grid_res[1]) * grid_res[1],
  to = ceiling(x_range[2] / grid_res[1]) * grid_res[1],
  by = grid_res[1]
)

# For the y-axis (BIO12)
y_range <- range(origin_dat_prepared$BIO12, na.rm = TRUE)
y_breaks <- seq(
  from = floor(y_range[1] / grid_res[2]) * grid_res[2],
  to = ceiling(y_range[2] / grid_res[2]) * grid_res[2],
  by = grid_res[2]
)
```

#### Combined Comparison Plot

``` r
ggplot() +
  # Plot the original data as a background layer
  geom_point(data = origin_dat_prepared, aes(x = BIO1, y = BIO12), 
             color = "#D55E00", alpha = 0.4, size = 2) +
  
  # Add the grid lines
  geom_vline(xintercept = x_breaks, color = "grey70", linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = y_breaks, color = "grey70", linetype = "dashed", linewidth = 0.5) +
  
  # Plot the stochastically thinned data on top
  geom_point(data = thinned_stochastic$thinned_data, aes(x = BIO1, y = BIO12), 
             color = "#0072B2", alpha = 0.7, size = 1.2) +
  
  # Add informative labels
  labs(
    title = "Stochastic Thinning: Thinned Points Overlaid on Original Data",
    subtitle = paste(nrow(thinned_stochastic$thinned_data), "points remaining (blue) from", nrow(origin_dat_prepared), "original points (orange)"),
    x = "Mean Annual Temperature (BIO1, scaled)",
    y = "Annual Precipitation (BIO12, scaled)"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())
```

<img src="man/figures/README-plot-combined-comparison-grid-1.png" width="100%" />

#### Create the Thinned Center Data with Grid

``` r
ggplot() +
  # 1. Plot the original data points as a faded background
  geom_point(data = origin_dat_prepared, 
             aes(x = BIO1, y = BIO12), 
             color = "grey40", alpha = 0.5, size = 1.5) + 
  # 2. Add the grid lines
  geom_vline(xintercept = x_breaks, color = "grey70", linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = y_breaks, color = "grey70", linetype = "dashed", linewidth = 0.5) +
  
  # 3. Plot the new grid cell centers on top
  geom_point(data = thinned_deterministic$thinned_points, 
             aes(x = BIO1, y = BIO12), 
             color = "#D55E00", size = 1.5, shape = 3, stroke = 1) +
  
  # 4. Add informative labels
  labs(
    title = "Deterministic Thinning to Grid Cell Centers",
    subtitle = paste(nrow(thinned_deterministic$thinned_points), 
                     "unique cell centers (orange crosses) from", 
                     nrow(thinned_deterministic$original_points), 
                     "original points (grey)"),
    x = "Mean Annual Temperature (BIO1)",
    y = "Annual Precipitation (BIO12)"
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())
```

<img src="man/figures/README-plot-thinned-center-grid-1.png" width="100%" />

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
                                    level = 95)
# The returned object contains all the details
# We can use the custom print() method for a clean summary
stochastic_ellipse
#> --- Bean Environmental Niche Ellipse ---
#> 
#> Method: 'covmat'.
#> Fitted to 1508 data points at a 95.00% percentage of data.
#> 1448 out of 1508 points (96.0%) fall within the ellipse boundary.
#> 
#> Niche Centroid:
#>       BIO1      BIO12 
#> -0.1258300 -0.2452734

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
                                       level = 95)
# The returned object contains all the details
# We can use the custom print() method for a clean summary
deterministic_ellipse
#> --- Bean Environmental Niche Ellipse ---
#> 
#> Method: 'covmat'.
#> Fitted to 371 data points at a 95.00% percentage of data.
#> 363 out of 371 points (97.8%) fall within the ellipse boundary.
#> 
#> Niche Centroid:
#>        BIO1       BIO12 
#> -0.08238699 -0.32555737

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
#> 1    0.734  0.012      0.735   0.701   0.763
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
#> t = -13.011, df = 197.54, p-value < 2.2e-16
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.02415648 -0.01779739
#> sample estimates:
#> mean of x mean of y 
#> 0.7128503 0.7338273

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

### The End ❤️
