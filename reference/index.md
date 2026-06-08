# Package index

## Data Preparation

Functions for preparing and cleaning occurrence data.

- [`prepare_bean()`](https://paanwaris.github.io/bean/reference/prepare_bean.md)
  : Prepare data for environmental thinning

## Thinning Logic

Functions for addressing sampling bias by thinning in environmental
space (E-space).

- [`find_env_resolution()`](https://paanwaris.github.io/bean/reference/find_env_resolution.md)
  : Find an objective environmental grid resolution
- [`thin_env_nd()`](https://paanwaris.github.io/bean/reference/thin_env_nd.md)
  : Thin occurrence data in n-dimensional environmental space
- [`thin_env_center()`](https://paanwaris.github.io/bean/reference/thin_env_center.md)
  : Deterministic centroid
- [`plot_bean()`](https://paanwaris.github.io/bean/reference/plot_bean.md)
  : Visualize n-dimensional environmental thinning results

## Niche delineation

Functions for fitting an environmental-niche ellipsoid.

- [`fit_ellipsoid()`](https://paanwaris.github.io/bean/reference/fit_ellipsoid.md)
  : Fit an environmental niche ellipsoid

## Datasets

Example data shipped with the package.

- [`occ_data_raw`](https://paanwaris.github.io/bean/reference/occ_data_raw.md)
  : Raw Rusa unicolor occurrence data
- [`origin_dat_prepared`](https://paanwaris.github.io/bean/reference/origin_dat_prepared.md)
  : Cleaned and scaled occurrence data
- [`thinned_stochastic`](https://paanwaris.github.io/bean/reference/thinned_stochastic.md)
  : Stochastically thinned environmental data
- [`thinned_deterministic`](https://paanwaris.github.io/bean/reference/thinned_deterministic.md)
  : Deterministically thinned environmental data
- [`origin_ellipse`](https://paanwaris.github.io/bean/reference/origin_ellipse.md)
  : Fitted niche ellipsoid for Rusa unicolor
