
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
#> Data preparation complete. Returning 1588 clean records.

# View the structure and summary of the clean, scaled data
head(origin_dat_prepared)
#>                  species          x        y        PC1        PC2        PC3
#> 1 Peromyscus maniculatus -119.47519 47.37757 -1.5213782  1.7219400 -1.1818238
#> 2 Peromyscus maniculatus -119.51685 34.42286  3.0321994  5.1812487 -1.1879184
#> 3 Peromyscus maniculatus  -77.40364 39.08822  0.8128774 -0.8775375 -0.1797138
#> 4 Peromyscus maniculatus -122.59932 45.58640  1.3011913  2.0415363 -2.2844498
#> 5 Peromyscus maniculatus -111.85233 34.83941 -0.3278183  2.0679471 -0.1780916
#> 6 Peromyscus maniculatus -117.22582 33.38149  4.2337761  4.3437796 -1.5741526
summary(origin_dat_prepared)
#>    species                x                 y              PC1         
#>  Length:1588        Min.   :-124.35   Min.   :30.05   Min.   :-5.4286  
#>  Class :character   1st Qu.:-122.14   1st Qu.:38.46   1st Qu.:-1.5608  
#>  Mode  :character   Median :-113.85   Median :41.84   Median : 0.4583  
#>                     Mean   :-105.49   Mean   :41.49   Mean   : 0.3091  
#>                     3rd Qu.: -87.60   3rd Qu.:44.92   3rd Qu.: 2.0539  
#>                     Max.   : -67.16   Max.   :48.96   Max.   : 6.2050  
#>       PC2               PC3         
#>  Min.   :-3.9383   Min.   :-9.4809  
#>  1st Qu.:-1.4649   1st Qu.:-2.6345  
#>  Median : 0.9103   Median :-1.1812  
#>  Mean   : 0.6884   Mean   :-1.7195  
#>  3rd Qu.: 2.2570   3rd Qu.:-0.4589  
#>  Max.   : 5.2180   Max.   : 2.5078
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
  env_vars = c("PC1", "PC2"),
  quantile = 0.90
)
#> Calculating pairwise distances for each environmental axis...

# The function returns a suggested resolution and the full distance distribution
resolution_results
#> --- Bean Environmental Resolution Analysis ---
#> 
#> Suggested Grid Resolutions (at the 90% quantile):
#>   - PC1: 5.635577
#>   - PC2: 5.337637
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
  env_vars = c("PC1", "PC2"),
  grid_resolution = 0.1,
  target_percent = 0.95
)
#> Searching for optimal cap...
#>   |                                                                              |                                                                      |   0%  |                                                                              |=====                                                                 |   8%  |                                                                              |===========                                                           |  15%  |                                                                              |================                                                      |  23%  |                                                                              |======================                                                |  31%  |                                                                              |===========================                                           |  38%  |                                                                              |================================                                      |  46%  |                                                                              |======================================                                |  54%  |                                                                              |===========================================                           |  62%  |                                                                              |================================================                      |  69%  |                                                                              |======================================================                |  77%  |                                                                              |===========================================================           |  85%  |                                                                              |=================================================================     |  92%  |                                                                              |======================================================================| 100%

# The function automatically saves results to the output directory.
# We can also inspect the returned list object.
# Print the recommendations
optimal_params
#> --- Bean Optimization Results ---
#> 
#> Target: Retain >= 1508 occurrence points.
#> 
#> Recommendation for 'Closest to Target':
#>   - Best Cap: 6
#>   - Retained Points: 1522 (Difference of 14)
#> 
#> Recommendation for 'Closest Above Target' (Recommended for use):
#>   - Best Cap: 6
#>   - Retained Points: 1522
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
  env_vars = c("PC1", "PC2"),
  grid_resolution = grid_res, 
  max_per_cell = chosen_cap
)

# Print the summary of the thinning results
thinned_stochastic
#> --- Bean Stochastic Thinning Results ---
#> 
#> Thinned 1588 original points to 31 points.
#> This represents a retention of 2.0% of the data.
#> 
#> --------------------------------------
head(thinned_stochastic$thinned_data)
#>                  species         x        y        PC1       PC2        PC3
#> 1 Peromyscus maniculatus -73.02987 42.42062 -1.5380454 -2.327994 -0.8726306
#> 2 Peromyscus maniculatus -72.44670 44.33675 -2.8062124 -2.560556 -0.7468526
#> 3 Peromyscus maniculatus -93.60744 41.50421 -1.0340203 -1.853637  1.1728551
#> 4 Peromyscus maniculatus -84.11010 46.25288 -1.6638094 -2.792764 -2.1373260
#> 5 Peromyscus maniculatus -85.52637 44.33675 -1.1875987 -2.297914 -1.3456343
#> 6 Peromyscus maniculatus -80.61108 40.17125 -0.2816871 -1.295985 -0.4289961
```

#### Method B: Deterministic Thinning with `thin_env_center`

This method provides a simpler, non-random alternative. It returns a
single new point at the exact center of every occupied grid cell,
regardless of how many points were originally in it.

``` r
# Apply the deterministic thinning
thinned_deterministic <- thin_env_center(
  data = origin_dat_prepared,
  env_vars = c("PC1", "PC2"),
  grid_resolution = grid_res
)

# Print the summary of the thinning results
thinned_deterministic
#> --- Bean Deterministic Thinning Results ---
#> 
#> Thinned 1588 original points to 6 unique grid cell centers.
#> 
#> -----------------------------------------
head(thinned_deterministic$thinned_points)
#>         PC1       PC2
#> 1 -2.817788  2.668818
#> 2  2.817788  2.668818
#> 3  2.817788 -2.668818
#> 4 -2.817788 -2.668818
#> 5  8.453365  2.668818
#> 6  8.453365 -2.668818
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
  env_vars = c("PC1", "PC2")
)

# Visualize the deterministic thinning results
plot_deterministic <- plot_bean(
  original_data = origin_dat_prepared,
  thinned_object = thinned_deterministic,
  grid_resolution = grid_res,
  env_vars = c("PC1", "PC2")
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
                                    env_vars = c("PC1", "PC2"), 
                                    method = "covmat", 
                                    level = 0.95)
# The returned object contains all the details
# We can use the custom print() method for a clean summary
stochastic_ellipse
#> --- Bean Environmental Niche Ellipse ---
#> 
#> Method: 'covmat'.
#> Fitted to 31 data points at a 95.00% level.
#> 31 out of 31 points (100.0%) fall within the ellipse boundary.
#> 
#> Niche Centroid:
#>       PC1       PC2 
#> 1.3882147 0.4502107

# And we can use the custom plot() method for a powerful visualization
plot(stochastic_ellipse)
```

<img src="man/figures/README-fit-ellipse-part1-1.png" width="100%" />

### Deterministic Thinned Ellipsoid

``` r
# Fit an ellipse that contains 95% of the thinned data
deterministic_ellipse <- fit_ellipsoid(data = thinned_deterministic$thinned_points,
                                       env_vars = c("PC1", "PC2"), 
                                       method = "covmat", 
                                       level = 0.95)
# The returned object contains all the details
# We can use the custom print() method for a clean summary
deterministic_ellipse
#> --- Bean Environmental Niche Ellipse ---
#> 
#> Method: 'covmat'.
#> Fitted to 6 data points at a 95.00% level.
#> 6 out of 6 points (100.0%) fall within the ellipse boundary.
#> 
#> Niche Centroid:
#>      PC1      PC2 
#> 2.817788 0.000000

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
background_points <- randomPoints(scale(raster::stack(env_pca)), 1000)
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
#> 1    0.798   0.01      0.798   0.761   0.826
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
#> 1     0.76  0.095      0.763   0.507   0.979
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
#> t = 3.9162, df = 101.3, p-value = 0.0001633
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  0.0185014 0.0564823
#> sample estimates:
#> mean of x mean of y 
#> 0.7979880 0.7604962

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

## Generalist Model Calibration

``` r
# Print the summary table to see the best combination of parameters
generalist_calibration
#> --- Bean Parameter Calibration Results ---
#> 
#> Search Summary (sorted by performance):
#> # A tibble: 41 × 6
#>    combination     mean_auc sd_auc p_value_vs_original significance group       
#>    <chr>              <dbl>  <dbl> <chr>               <chr>        <chr>       
#>  1 q0.05_mve          0.804  0.01  0.3049              ns           b           
#>  2 Original_mve       0.804  0.009 0.8354              ns           abd         
#>  3 q0.40_mve          0.802  0.012 0.9570              ns           ab          
#>  4 q0.50_mve          0.802  0.011 0.9677              ns           ab          
#>  5 q0.55_mve          0.802  0.01  0.9890              ns           abd         
#>  6 q0.65_mve          0.802  0.011 0.9896              ns           abd         
#>  7 q0.10_mve          0.801  0.011 0.9976              ns           abcd        
#>  8 q0.35_covmat       0.801  0.009 0.9996              ns           abcdh       
#>  9 q0.60_mve          0.801  0.011 0.9999              ns           abcdh       
#> 10 q0.25_mve          0.801  0.01  1.0000              ns           abcdhj      
#> 11 q0.20_mve          0.8    0.013 1.0000              ns           abcdhj      
#> 12 q0.35_mve          0.8    0.013 1.0000              ns           abcdhjk     
#> 13 q0.20_covmat       0.799  0.01  1.0000              ns           abcdhijk    
#> 14 q0.05_covmat       0.799  0.011 1.0000              ns           acdehijk    
#> 15 q0.65_covmat       0.799  0.01  1.0000              ns           acdehijk    
#> 16 q0.30_mve          0.798  0.011 1.0000              ns           acdehijk    
#> 17 q0.70_mve          0.798  0.011 1.0000              ns           acdehijk    
#> 18 Original_covmat    0.798  0.01  1.0000              ns           abcdefghijkl
#> 19 Original           0.798  0.011 NA                  NA           abcdefghijk…
#> 20 q0.10_covmat       0.798  0.01  1.0000              ns           acdehijk    
#> 21 q0.40_covmat       0.798  0.01  1.0000              ns           acdehijk    
#> 22 q0.55_covmat       0.798  0.01  1.0000              ns           acdehijk    
#> 23 q0.60_covmat       0.797  0.011 1.0000              ns           acdefhijk   
#> 24 q0.25_covmat       0.797  0.01  1.0000              ns           cdefhijk    
#> 25 q0.70_covmat       0.797  0.01  1.0000              ns           cefhijk     
#> 26 q0.15_covmat       0.797  0.011 1.0000              ns           cefhijk     
#> 27 q0.45_covmat       0.796  0.01  1.0000              ns           efhijkl     
#> 28 q0.45_mve          0.796  0.011 1.0000              ns           efhijkl     
#> 29 q0.50_covmat       0.796  0.011 1.0000              ns           efgijklm    
#> 30 q0.75_covmat       0.795  0.011 0.9999              ns           efgiklmn    
#> 31 q0.90_mve          0.795  0.011 0.9998              ns           efgilmn     
#> 32 q0.80_mve          0.794  0.013 0.9962              ns           efglmn      
#> 33 q0.30_covmat       0.794  0.01  0.9957              ns           efglmn      
#> 34 q0.75_mve          0.794  0.01  0.9807              ns           efglmn      
#> 35 q0.85_mve          0.793  0.011 0.4691              ns           fglmn       
#> 36 q0.95_mve          0.792  0.01  0.1392              ns           glmn        
#> 37 q0.15_mve          0.791  0.009 0.0768              ns           gmn         
#> 38 q0.80_covmat       0.791  0.012 0.0642              ns           mn          
#> 39 q0.90_covmat       0.791  0.011 0.0425              *            n           
#> 40 q0.95_covmat       0.791  0.01  0.0275              *            n           
#> 41 q0.85_covmat       0.79   0.01  0.0085              **           n           
#> 
#> --- Best Combination ---
#> Optimal Quantile: 0.050
#> Optimal Ellipse Method: 'mve'
#> Resulting Grid Resolution:
#>   - PC1: 0.2020
#>   - PC2: 0.1748
#> Resulting Thinning Cap: 9
#> ---
#> Significance stars (*) indicate p-value from a pairwise comparison against the 'Original' baseline model.
#> Signif. codes: '***' p < 0.001,  '**' p < 0.01,  '*' p < 0.05,  'ns' p >= 0.05
#> Models sharing a letter in the 'group' column are not significantly different from each other (Tukey's HSD).

# Plot the results to visualize the performance trade-offs
plot(generalist_calibration)
```

<img src="man/figures/README-calibrate-bean-2-1.png" width="100%" />

``` r

# You can now access the final, best-thinned data directly for your final model
generalist_data <- generalist_calibration$best_points_in_ellipse
head(generalist_data)
#>                  species         x        y       PC1       PC2        PC3
#> 1 Peromyscus maniculatus -69.53085 44.42006 -1.904111 -1.892901 -1.1684538
#> 2 Peromyscus maniculatus -73.23814 43.67027 -1.849580 -2.072949 -1.2571576
#> 3 Peromyscus maniculatus -69.65581 44.54503 -1.994636 -2.004637 -1.3451039
#> 4 Peromyscus maniculatus -69.65581 44.54503 -1.994636 -2.004637 -1.3451039
#> 5 Peromyscus maniculatus -89.19201 43.21207 -2.002375 -2.189141  0.6783397
#> 6 Peromyscus maniculatus -72.53001 43.12876 -1.935360 -2.343008 -1.1830547
```

## Specialist Model Calibration

``` r
# Print the summary table to see the best combination of parameters
specialist_calibration
#> --- Bean Parameter Calibration Results ---
#> 
#> Search Summary (sorted by performance):
#> # A tibble: 41 × 6
#>    combination     mean_auc sd_auc p_value_vs_original significance group
#>    <chr>              <dbl>  <dbl> <chr>               <chr>        <chr>
#>  1 Original_covmat    0.988  0.003 < 2e-16             ***          a    
#>  2 q0.95_covmat       0.988  0.003 < 2e-16             ***          a    
#>  3 q0.80_covmat       0.987  0.003 < 2e-16             ***          a    
#>  4 q0.25_covmat       0.987  0.003 < 2e-16             ***          a    
#>  5 q0.20_covmat       0.987  0.003 < 2e-16             ***          a    
#>  6 q0.15_covmat       0.987  0.003 < 2e-16             ***          a    
#>  7 q0.10_covmat       0.987  0.003 < 2e-16             ***          a    
#>  8 q0.45_covmat       0.987  0.003 < 2e-16             ***          a    
#>  9 q0.05_covmat       0.987  0.004 < 2e-16             ***          a    
#> 10 q0.65_covmat       0.987  0.003 < 2e-16             ***          a    
#> 11 q0.90_covmat       0.987  0.003 < 2e-16             ***          a    
#> 12 q0.85_covmat       0.987  0.003 < 2e-16             ***          a    
#> 13 q0.55_covmat       0.987  0.003 < 2e-16             ***          a    
#> 14 q0.35_covmat       0.987  0.004 < 2e-16             ***          a    
#> 15 q0.60_covmat       0.987  0.003 < 2e-16             ***          a    
#> 16 q0.30_covmat       0.987  0.003 < 2e-16             ***          a    
#> 17 q0.70_covmat       0.987  0.004 < 2e-16             ***          a    
#> 18 q0.75_covmat       0.987  0.004 < 2e-16             ***          a    
#> 19 q0.40_covmat       0.987  0.003 < 2e-16             ***          a    
#> 20 q0.50_covmat       0.987  0.003 < 2e-16             ***          a    
#> 21 q0.95_mve          0.983  0.004 < 2e-16             ***          b    
#> 22 q0.20_mve          0.983  0.003 < 2e-16             ***          b    
#> 23 q0.45_mve          0.983  0.004 < 2e-16             ***          b    
#> 24 q0.15_mve          0.983  0.004 < 2e-16             ***          b    
#> 25 q0.05_mve          0.983  0.004 < 2e-16             ***          b    
#> 26 q0.40_mve          0.983  0.004 < 2e-16             ***          b    
#> 27 q0.50_mve          0.983  0.004 < 2e-16             ***          b    
#> 28 q0.35_mve          0.983  0.004 2.1e-12             ***          b    
#> 29 q0.85_mve          0.983  0.004 4.4e-12             ***          b    
#> 30 q0.25_mve          0.983  0.004 6.3e-12             ***          b    
#> 31 Original_mve       0.983  0.004 3.5e-07             ***          b    
#> 32 q0.65_mve          0.983  0.004 1.3e-11             ***          b    
#> 33 q0.10_mve          0.983  0.004 1.7e-11             ***          b    
#> 34 q0.80_mve          0.983  0.004 2.2e-11             ***          b    
#> 35 q0.55_mve          0.983  0.004 3.7e-11             ***          b    
#> 36 q0.30_mve          0.982  0.004 1.5e-10             ***          b    
#> 37 q0.75_mve          0.982  0.004 3.7e-10             ***          b    
#> 38 q0.60_mve          0.982  0.005 3.4e-09             ***          b    
#> 39 q0.90_mve          0.982  0.004 1.4e-08             ***          b    
#> 40 q0.70_mve          0.982  0.004 3.6e-08             ***          b    
#> 41 Original           0.978  0.006 NA                  NA           c    
#> 
#> --- Best Combination ---
#> Optimal Quantile: 0.950
#> Optimal Ellipse Method: 'covmat'
#> Resulting Grid Resolution:
#>   - PC1: 1.7648
#>   - PC2: 2.3352
#> Resulting Thinning Cap: 50
#> ---
#> Significance stars (*) indicate p-value from a pairwise comparison against the 'Original' baseline model.
#> Signif. codes: '***' p < 0.001,  '**' p < 0.01,  '*' p < 0.05,  'ns' p >= 0.05
#> Models sharing a letter in the 'group' column are not significantly different from each other (Tukey's HSD).

# Plot the results to visualize the performance trade-offs
plot(specialist_calibration)
```

<img src="man/figures/README-calibrate-bean-4-1.png" width="100%" />

``` r

# You can now access the final, best-thinned data directly for your final model
specialist_data <- specialist_calibration$best_points_in_ellipse
head(specialist_data)
#>           species        y         x       PC1       PC2       PC3
#> 3 Lynx canadensis 44.80376 -70.76662 -2.766962 -2.234711 -1.775255
#> 4 Lynx canadensis 45.79533 -69.34596 -2.791968 -2.305363 -1.742363
#> 5 Lynx canadensis 45.99159 -69.21948 -2.882799 -2.314592 -1.611886
#> 6 Lynx canadensis 46.06965 -68.47697 -3.387130 -2.333576 -1.113026
#> 7 Lynx canadensis 46.12326 -68.57641 -3.339733 -2.328240 -1.136800
#> 8 Lynx canadensis 46.16111 -69.17126 -2.963235 -2.277617 -1.471623
```

### The End ❤️
