# Visualize n-dimensional environmental thinning results

This function creates a scatterplot matrix (pairs plot) to visualize the
results of n-dimensional environmental thinning using base R graphics.
It can accept thinned objects from either density-based thinning
(\`thin_env_nd\`) or deterministic centroid thinning
(\`thin_env_center\`).

## Usage

``` r
plot_bean(original_data, thinned_object, env_vars)
```

## Arguments

- original_data:

  A data.frame of the prepared, unthinned occurrence points.

- thinned_object:

  The output object from \`thin_env_nd()\` or \`thin_env_center()\`.

- env_vars:

  A character vector of the environmental variables to plot.

## Value

Invisibly returns \`NULL\`. Draws a plot to the active graphics device.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assume 'prepared_data' is ready.

# --- Example: Plotting Density-Based Thinning ---
thinned_obj <- thin_env_nd(
  data = prepared_data,
  env_vars = c("PC1", "PC2", "PC3"),
  grid_resolution = c(0.5, 0.5, 0.5)
)

plot_bean(
  original_data = prepared_data,
  thinned_object = thinned_obj,
  env_vars = c("PC1", "PC2", "PC3")
)
} # }
```
