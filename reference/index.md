# Package index

## Data Preparation

Functions for preparing and cleaning occurrence data.

- [`prepare_bean()`](https://paanwaris.github.io/bean/reference/prepare_bean.md)
  : Prepare data for environmental thinning

## Thinning Logic

Functions for addressing sampling bias by thinning in environmental
space (E-space).

- [`find_env_resolution()`](https://paanwaris.github.io/bean/reference/find_env_resolution.md)
  : Find objective environmental resolution using the Nearest Neighbor
  Elbow Method
- [`thin_env_nd()`](https://paanwaris.github.io/bean/reference/thin_env_nd.md)
  : Thin occurrence data in n-dimensional environmental space
- [`thin_env_center()`](https://paanwaris.github.io/bean/reference/thin_env_center.md)
  : Deterministic centroid
- [`plot_bean()`](https://paanwaris.github.io/bean/reference/plot_bean.md)
  : Visualize n-dimensional environmental thinning results

## Modeling & Visualization

Functions for fundamental niche delineation and predicting suitability.

- [`fit_ellipsoid()`](https://paanwaris.github.io/bean/reference/fit_ellipsoid.md)
  : Outlier removal
- [`predict(`*`<bean_ellipsoid>`*`)`](https://paanwaris.github.io/bean/reference/predict.ellipsoid_bean.md)
  : Predict suitability and Mahalanobis distance from a bean ellipsoid
