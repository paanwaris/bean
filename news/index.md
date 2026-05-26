# Changelog

## bean 0.2.0

### Major changes

- [`find_env_resolution()`](https://paanwaris.github.io/bean/reference/find_env_resolution.md)
  no longer uses a geometric “elbow” rule on nearest- neighbour
  distances. It now selects a kernel-density bandwidth for each
  environmental variable using the Sheather-Jones plug-in estimator
  (default), with Silverman and Scott rules also available via the new
  `method` argument. The bandwidth is a statistically defensible choice
  for the edge length of an environmental grid cell, and the new
  implementation is faster and more robust to ties.
- `rgl` is now a *Suggests* dependency rather than an *Imports*. 3-D
  plots still work when `rgl` is installed; otherwise
  [`plot.bean_ellipsoid()`](https://paanwaris.github.io/bean/reference/plot.bean_ellipsoid.md)
  falls back to a 2-D view of the first two requested dimensions.
- The S3 method
  [`predict.bean_ellipsoid()`](https://paanwaris.github.io/bean/reference/predict.bean_ellipsoid.md)
  is now documented under its canonical name (previously the help page
  was generated as `predict.ellipsoid_bean`).
- Vignettes have been reorganised:
  - `bean-overview` — quickstart introduction.
  - `data-preparation` — preparing rasters and occurrences.
  - `environmental-thinning` — resolution selection and thinning.
  - `niche-modeling` — fitting ellipsoids and projecting suitability.
    All vignettes now build without requiring `rgl`.

### CRAN readiness

- `DESCRIPTION`: trimmed `Imports` to `MASS`, `stats`, `terra`; moved
  `rgl`, `ggplot2` and `dplyr` to `Suggests` (with `dplyr` removed
  entirely from the package — it was only used in one vignette).
- Added a `tests/testthat/` test suite covering
  [`find_env_resolution()`](https://paanwaris.github.io/bean/reference/find_env_resolution.md),
  [`thin_env_nd()`](https://paanwaris.github.io/bean/reference/thin_env_nd.md),
  [`thin_env_center()`](https://paanwaris.github.io/bean/reference/thin_env_center.md),
  [`fit_ellipsoid()`](https://paanwaris.github.io/bean/reference/fit_ellipsoid.md)
  and
  [`predict.bean_ellipsoid()`](https://paanwaris.github.io/bean/reference/predict.bean_ellipsoid.md).
- Datasets are now documented as user-visible (not
  `\keyword{internal}`).
- Replaced `\dontrun{}` examples that depended on missing files with
  `\donttest{}` examples that use the shipped sample data.
- [`prepare_bean()`](https://paanwaris.github.io/bean/reference/prepare_bean.md)
  now uses [`match.arg()`](https://rdrr.io/r/base/match.arg.html) for
  `transform`.
- Cleaned up `R/globals.R` (was carrying many unused identifiers).

### Minor

- `print.bean_ellipsoid()` output reformatted for clarity.
- Internal helper `.bean_ellipse_polygon()` consolidates the 2-D ellipse
  polygon code path used by both
  [`fit_ellipsoid()`](https://paanwaris.github.io/bean/reference/fit_ellipsoid.md)
  and
  [`plot.bean_ellipsoid()`](https://paanwaris.github.io/bean/reference/plot.bean_ellipsoid.md).

## bean 0.1.2

- Earlier development versions; see commit history on GitHub.
