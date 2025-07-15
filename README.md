
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bean

<!-- badges: start -->

[![R-CMD-check](https://github.com/your-github-username/bean/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/your-github-username/bean/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `bean` is to reduce sampling bias in species occurrence data
by thinning points that are clustered in environmental space. This is a
common and necessary pre-processing step for many ecological niche
modeling (ENM) workflows.

The package provides two main functions that work together:

1.  `find_optimal_cap()`: Helps you choose a thinning intensity by
    searching for the density cap (`max_per_cell`) that best achieves a
    target percentage of data retention.
2.  `thin_env_density()`: Applies the thinning using the parameters
    you’ve chosen.

## Installation

You can install the development version of `bean` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("your-github-username/bean")
```

## Example Workflow

This example demonstrates the core workflow of `bean` using a dataset
for the deer mouse, *Peromyscus maniculatus*. The recommended process is
to first use `find_optimal_cap()` to make a defensible choice for your
thinning parameters, and then use `thin_env_density()` to apply them.

### 1. Load Libraries and Data

First, we load `bean`, `dplyr` for data manipulation, and `ggplot2` for
visualization. We will use the `P_maniculatus.csv` dataset, which
contains occurrence records along with two key environmental variables:
`BIO1` (Mean Annual Temperature) and `BIO12` (Annual Precipitation).

``` r
library(bean)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)

# Load the occurrence data
# In a real package, you would load this via data(P_maniculatus)
# For this example, we'll read it directly.
occ_data <- read.csv("data/P_maniculatus_samples.csv")

# Inspect the data
occ_data <- occ_data %>%
  filter(!is.na(BIO1) & !is.na(BIO12))
head(occ_data)
#>   X       BIO1       BIO2        BIO3       BIO4       BIO5       BIO6
#> 1 1 -1.4532242  0.2093895 -1.25028926  1.4868788 -1.2801579 -1.5555510
#> 2 2 -0.8089705 -0.1140447 -0.38563031  0.4086082 -1.4298291 -0.8036575
#> 3 3 -0.7874954 -0.1140447 -0.38563031  0.3989951 -1.4048839 -0.7805223
#> 4 4 -1.8397765  0.5004803  0.04669916  0.1666783 -1.7541167 -1.1159825
#> 5 5 -1.3028984 -0.2757618 -1.25028926  1.1990196 -1.3300483 -1.1159825
#> 6 6 -0.3150427 -0.9873171 -0.38563031 -0.8934342 -0.1077337  0.5497509
#>         BIO7      BIO10      BIO11       BIO12      BIO13      BIO14      BIO15
#> 1  1.0605147 -1.3376190 -1.6452726 -0.35902605 -0.1057818 -1.0525058  0.7984393
#> 2  0.1550299 -1.0101018 -0.7985610  0.08377747  0.3391621 -0.5027304  0.7224502
#> 3  0.1422766 -0.9828087 -0.7698589  0.09254586  0.3454289 -0.4909072  0.7203555
#> 4  0.3335762 -2.1837049 -1.2290923 -1.18763858 -1.2087413 -0.6859888 -0.9703739
#> 5  0.5503824 -1.3103259 -1.4587090 -0.30641573  0.0947563 -1.0111248  1.0056930
#> 6 -0.6611818 -0.8736363  0.1629590 -0.41602056 -0.8327324  0.1889226 -1.4147882
#>         BIO16      BIO17          x        y
#> 1 -0.08591658 -1.1162098  -90.10842 45.79468
#> 2  0.30954512 -0.2943088  -72.23842 42.46228
#> 3  0.31592354 -0.2832766  -72.23842 42.42062
#> 4 -1.17981468 -0.8348880 -105.47911 40.21291
#> 5  0.08630061 -1.0996615  -89.56690 46.25288
#> 6 -0.84175871  0.2131737 -122.01615 45.83633

# Visualize the Original Data
ggplot(occ_data, aes(x = BIO1, y = BIO12)) +
  geom_point(alpha = 0.5, color = "darkred") +
  labs(
    title = "Original Occurrence Points in Environmental Space",
    subtitle = paste(nrow(occ_data), "total points"),
    x = "Mean Annual Temperature (BIO1)",
    y = "Annual Precipitation (BIO12)",
    caption = "Data for Peromyscus maniculatus"
  ) +
  theme_bw()
```

<img src="man/figures/README-setup-1.png" width="100%" />

### 2. Find an Optimal Thinning Level

Instead of guessing a `max_per_cell` value, we can use
`find_optimal_cap()` to find the cap that gets us closest to a desired
retention rate. Let’s say our goal is to retain about 80% of our
original data. The `grid_resolution` parameter defines the size of the
grid cells in the units of the environmental variables.

``` r
set.seed(81) # for reproducibility

# Define the grid resolution
grid_res <- 0.01 # A resolution grid of 0.01 unit

# Let's target retaining 50% of the data
optimal_params <- find_optimal_cap(
  data = occ_data,
  env_vars = c("BIO1", "BIO12"),
  grid_resolution = grid_res,
  target_percent = 0.50
)

# Print the recommendations
optimal_params[1:4]
#> $best_cap
#> [1] 102
#> 
#> $retained_points
#> [1] 4993
#> 
#> $search_results
#> # A tibble: 586 × 2
#>      cap thinned_count
#>    <int>         <int>
#>  1     1           435
#>  2     2           545
#>  3     3           628
#>  4     4           704
#>  5     5           774
#>  6     6           843
#>  7     7           911
#>  8     8           979
#>  9     9          1047
#> 10    10          1115
#> # ℹ 576 more rows
#> 
#> $<NA>
#> NULL

# Visualize the search process
optimal_params$plot
#> NULL
```

The plot and the output list show that to get closest to our target of
80%, we should use a cap of . For this demonstration, we’ll use the
`best_cap_conservative` recommendation to ensure we don’t remove too
much data.

### 3. Apply Environmental Thinning

Now we use the `thin_env_density()` function with the `max_per_cell`
parameter we just determined.

``` r
chosen_cap <- optimal_params$best_cap_conservative
cat(sprintf("Proceeding with the 'conservative' cap: %d\n", chosen_cap))

thinned_data <- thin_env_density(
  data = occ_data,
  env_vars = c("BIO1", "BIO12"),
  grid_resolution = grid_res, 
  max_per_cell = 104
)

cat(sprintf("Original number of points: %d\n", nrow(occ_data)))
#> Original number of points: 10000
cat(sprintf("Thinned number of points:  %d\n", nrow(thinned_data)))
#> Thinned number of points:  5049
cat(sprintf("Percentage of points removed: %.1f%%\n", 100 * (1 - nrow(thinned_data) / nrow(occ_data))))
#> Percentage of points removed: 49.5%
```

### 4. Visualize the Thinning Process with Grids

To see exactly what the function is doing, we can draw the environmental
grid over our plots.

``` r
# --- Calculate Grid Line Positions ---
# The grid lines correspond to the 'grid_resolution' parameter.

# For the x-axis (BIO1)
x_range <- range(occ_data$BIO1, na.rm = TRUE)
x_breaks <- seq(
  from = floor(x_range[1] / grid_res) * grid_res,
  to = ceiling(x_range[2] / grid_res) * grid_res,
  by = grid_res
)

# For the y-axis (BIO12)
y_range <- range(occ_data$BIO12, na.rm = TRUE)
y_breaks <- seq(
  from = floor(y_range[1] / grid_res) * grid_res,
  to = ceiling(y_range[2] / grid_res) * grid_res,
  by = grid_res
)

# --- Create a Labeled Data Frame for Comparison ---
# We create a unique ID to robustly identify which points were kept.
occ_data_labeled <- occ_data %>%
  mutate(unique_id = paste(x, y, sep = "_"))

thinned_data_labeled <- thinned_data %>%
  mutate(unique_id = paste(x, y, sep = "_"))
```

Now, let’s create the plots with the grid overlay.

#### Original Data with Grid

This plot shows the initial clustering of points within the
environmental grid cells.

``` r
ggplot(occ_data, aes(x = BIO1, y = BIO12)) +
  geom_vline(xintercept = x_breaks, color = "grey70", linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = y_breaks, color = "grey70", linetype = "dashed", linewidth = 0.5) +
  geom_point(color = "#D55E00", alpha = 0.6, size = 1.5) +
  labs(
    title = "Original Points with Environmental Grid",
    subtitle = paste(nrow(occ_data), "total points"),
    x = "Mean Annual Temperature (BIO1)",
    y = "Annual Precipitation (BIO12)"
  ) +
  theme_bw()
```

<img src="man/figures/README-plot-original-grid-1.png" width="100%" />

#### Thinned Data with Grid

This plot shows the result: a maximum of point(s) per cell.

``` r
ggplot(thinned_data, aes(x = BIO1, y = BIO12)) +
  geom_vline(xintercept = x_breaks, color = "grey70", linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = y_breaks, color = "grey70", linetype = "dashed", linewidth = 0.5) +
  geom_point(color = "#0072B2", alpha = 0.6, size = 1.5) +
  labs(
    title = "Thinned Occurrence Points on Environmental Grid",
    subtitle = paste(nrow(thinned_data), "points remaining (max", chosen_cap, "per cell)"),
    x = "Mean Annual Temperature (BIO1)",
    y = "Annual Precipitation (BIO12)"
  ) +
  theme_bw()
```

<img src="man/figures/README-plot-thinned-grid-1.png" width="100%" />

This workflow demonstrates how `bean` can be used to make a data-driven
choice about thinning parameters and then apply that thinning as a
pre-processing step before building species distribution models.
