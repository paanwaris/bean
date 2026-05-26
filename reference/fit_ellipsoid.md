# Fit an environmental niche ellipsoid

Fits an ellipsoid that encompasses a chosen proportion of the data
points in an environmental space of two or more dimensions. The centroid
and covariance matrix can be estimated either by the classical sample
moments (`"covmat"`) or by the robust Minimum Volume Ellipsoid (`"mve"`;
Rousseeuw, 1985). Points are classified as inside or outside the
ellipsoid using a \\\chi^2\\ cutoff on their squared Mahalanobis
distance.

## Usage

``` r
fit_ellipsoid(data, env_vars, method = "covmat", level = 0.95)
```

## Arguments

- data:

  A `data.frame` containing the environmental variables.

- env_vars:

  A character vector of at least two column names in `data` representing
  the environmental variables.

- method:

  One of `"covmat"` (default, classical) or `"mve"` (robust Minimum
  Volume Ellipsoid via
  [`cov.mve`](https://rdrr.io/pkg/MASS/man/cov.rob.html)).

- level:

  A single number in `(0, 1)`: the confidence level of the ellipsoid.
  Default `0.95`.

## Value

An object of class `bean_ellipsoid` (a list) with:

- `centroid`:

  Named vector of variable means / centre.

- `covariance_matrix`:

  The covariance matrix used.

- `niche_ellipse`:

  A `data.frame` of polygon vertices for the 2-D ellipse. `NULL` when
  more than two variables are supplied (the 3-D mesh is generated lazily
  on plot).

- `all_points_used`:

  Complete-case input data.

- `points_in_ellipse`:

  Subset inside the ellipsoid.

- `points_outside_ellipse`:

  Subset outside the ellipsoid.

- `inside_indices`:

  Row indices (in `all_points_used`) classified as inside.

- `parameters`:

  List with `level` and `method`.

## Details

**Methods.** `"covmat"` uses the sample mean and sample covariance
matrix. It is optimal under multivariate normality but sensitive to
outliers. `"mve"` (Rousseeuw, 1985) finds the smallest-volume ellipsoid
that contains a fraction of the data and is robust to a moderate
proportion of contaminating points.

**Confidence level.** Assuming approximate multivariate normality, the
boundary of the ellipsoid is the set of points whose squared Mahalanobis
distance equals `qchisq(level, df = n_dim)`.

## References

Rousseeuw, P. J. (1985). Multivariate estimation with high breakdown
point. In *Mathematical Statistics and Applications, Vol. B*, 283–297.

Van Aelst, S. & Rousseeuw, P. (2009). Minimum volume ellipsoid. *Wiley
Interdisciplinary Reviews: Computational Statistics*, 1(1), 71–82.

Cobos, M. E., Osorio-Olvera, L., Soberón, J., Peterson, A. T., Barve, V.
& Barve, N. (2024). ellipsenm: ecological niches' characterizations
using ellipsoids. <https://github.com/marlonecobos/ellipsenm>.

## Examples

``` r
set.seed(81)
env_data <- data.frame(
  BIO1  = c(rnorm(50, 10, 1), 30),
  BIO12 = c(rnorm(50, 20, 2), 50)
)
fit <- fit_ellipsoid(env_data, env_vars = c("BIO1", "BIO12"),
                     method = "covmat", level = 0.95)
print(fit)
#> -- Bean Environmental Niche Ellipsoid --
#> Method      : covmat
#> Dimensions  : 2 (BIO1, BIO12)
#> Level       : 95.00%
#> Points used : 51  (inside: 49, 96.1%)
#> Centroid:
#>     BIO1    BIO12 
#> 10.48281 20.99832 
plot(fit)

```
