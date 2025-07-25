% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_ellipsoid.R
\name{fit_ellipsoid}
\alias{fit_ellipsoid}
\title{Outlier removal}
\usage{
fit_ellipsoid(data, env_vars, method = "covmat", level = 95)
}
\arguments{
\item{data}{A data.frame containing species occurrence coordinates and the environmental variables.}

\item{env_vars}{A character vector specifying the column names in data that represent the environmental variables to be used in the analysis.}

\item{method}{(character) The method for calculating the centroid and
covariance matrix. Options are "covmat" (standard covariance) and "mve"
(Minimum Volume Ellipsoid, robust to outliers). Default = "covmat". See Details}

\item{level}{(numeric) A single value between 0 and 100 representing the
percentage of data points the ellipse is intended to encompass.
Default is 95.}
}
\value{
An object of class \code{bean_ellipsoid}, which is a list containing:
  \item{niche_ellipse}{A data.frame of points defining the perimeter of the calculated ellipse.}
  \item{centroid}{A named vector representing the center of the ellipse.}
  \item{covariance_matrix}{The 2x2 covariance matrix used to define the ellipse's shape.}
  \item{all_points_used}{The input data frame, filtered to include only complete, finite observations.}
  \item{points_in_ellipse}{A data.frame containing the subset of rows from \code{all_points_used} that fall inside the ellipse boundary.}
  \item{points_outside_ellipse}{A data.frame containing the subset of rows from \code{all_points_used} that fall outside the ellipse boundary.}
  \item{inside_indices}{A numeric vector of the row indices (from \code{all_points_used}) of the points inside the ellipse.}
  \item{parameters}{A list of the key parameters used, including \code{level} and \code{method}.}
}
\description{
This function calculates a bivariate ellipse that encompasses a
specified proportion of the data points in a 2D environmental space. It can
use either a standard covariance matrix or a robust Minimum Volume Ellipsoid.
The function also correctly identifies which of the input points fall within
and outside the calculated ellipse boundary, preserving all original columns.
}
\details{
This function provides two distinct statistical approaches for defining the
center and shape of the ellipse, selectable via the \code{method} parameter. The
size of the ellipse is controlled by the \code{level} parameter, which defines a
statistical confidence interval.

## Method

The \code{method} argument determines how the centroid and covariance
matrix (shape and orientation) of the data cloud are calculated.

 - "covmat" (Default): This is the classical approach, which uses the
    standard sample mean and sample covariance matrix calculated from all
    provided data points. While optimal for cleanly distributed, multivariate
    normal data, this method is highly sensitive to outliers. A single
    anomalous data point can significantly skew the mean and inflate the
    covariance matrix, resulting in an ellipse that poorly represents the
    central tendency of the data (Rousseeuw & Leroy, 2003).

 - "mve": This option uses the Minimum Volume Ellipsoid (MVE) estimator,
    a robust statistical method designed to resist the influence of outliers
    (Van Aelst & Rousseeuw, 2009). Instead of using all data points, the MVE algorithm finds the
    ellipsoid with the smallest possible volume that contains a specified
    subset of the data (at least h = (n_points + n_variables + 1)/2 points)
    (Cobos et al., 2024). By focusing on the most concentrated "core" of the data,
    the MVE method effectively ignores outliers, providing a more reliable
    estimate of the data's true center and scatter when contamination is
    present (Van Aelst & Rousseeuw, 2009).

## Confidence Level

The \code{level} parameter specifies the confidence level for the ellipse,
representing the percentage of the data that the ellipse is intended to
encompass(Cobos et al., 2024). It determines the size of the ellipse by defining a
statistical boundary based on Mahalanobis distances from the centroid.

Assuming the data follows a multivariate normal distribution, the boundary of
the ellipse corresponds to a quantile of the chi-squared (\eqn{\chi^2})
distribution (Van Aelst & Rousseeuw, 2009). For example, a \code{level} of 95 (the default)
constructs an ellipse whose boundary is defined by the set of points having a
squared Mahalanobis distance equal to the 0.95 quantile of the \eqn{\chi^2}
distribution with 2 degrees of freedom (for a 2D analysis). Points with a
smaller Mahalanobis distance are inside the ellipse, while those with a
larger distance are outside.

A higher \code{level} (e.g., 99) will result in a larger ellipse, while a lower
\code{level} (e.g., 90) will produce a smaller, more conservative ellipse.
}
\examples{
\dontrun{
# 1. Create environmental data with a cluster and an outlier
set.seed(81)
env_data <- data.frame(
  BIO1 = c(rnorm(50, mean = 10, sd = 1), 30),
  BIO12 = c(rnorm(50, mean = 20, sd = 2), 50)
)

# 2. Fit a 95\% ellipse using the standard covariance method
fit <- fit_ellipsoid(
  data = env_data,
  env_vars = c("BIO1", "BIO12"),
  method = "covmat",
  level = 95
)

# 3. Print the summary and plot the results
print(fit)
plot(fit)
}
}
\references{
Rousseeuw, P. J., & Leroy, A. M. (2003). Robust regression and outlier detection. John wiley & sons.

Van Aelst, S., & Rousseeuw, P. (2009). Minimum volume ellipsoid. Wiley Interdisciplinary Reviews: Computational Statistics, 1(1), 71-82.

Cobos, M.E., Osorio-Olvera, L., Soberón, J., Peterson, A.T., Barve, V. & Barve, N. (2024) ellipsenm: ecological niche’s characterizations using ellipsoids. <https://github.com/marlonecobos/ellipsenm>
}
