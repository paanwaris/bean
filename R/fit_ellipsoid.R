#' Outlier removal
#'
#' @description This function calculates a bivariate ellipse that encompasses a
#' specified proportion of the data points in a 2D environmental space. It can
#' use either a standard covariance matrix or a robust Minimum Volume Ellipsoid.
#' The function also correctly identifies which of the input points fall within
#' and outside the calculated ellipse boundary, preserving all original columns.
#'
#' @param data A data.frame containing species occurrence coordinates and the environmental variables.
#' @param env_vars A character vector specifying the column names in data that represent the environmental variables to be used in the analysis.
#' @param method (character) The method for calculating the centroid and
#'   covariance matrix. Options are "covmat" (standard covariance) and "mve"
#'   (Minimum Volume Ellipsoid, robust to outliers). Default = "covmat". See Details
#' @param level (numeric) A single value between 0 and 100 representing the
#'   percentage of data points the ellipse is intended to encompass.
#'   Default is 95.
#' @return An object of class \code{bean_ellipsoid}, which is a list containing:
#'   \item{niche_ellipse}{A data.frame of points defining the perimeter of the calculated ellipse.}
#'   \item{centroid}{A named vector representing the center of the ellipse.}
#'   \item{covariance_matrix}{The 2x2 covariance matrix used to define the ellipse's shape.}
#'   \item{all_points_used}{The input data frame, filtered to include only complete, finite observations.}
#'   \item{points_in_ellipse}{A data.frame containing the subset of rows from \code{all_points_used} that fall inside the ellipse boundary.}
#'   \item{points_outside_ellipse}{A data.frame containing the subset of rows from \code{all_points_used} that fall outside the ellipse boundary.}
#'   \item{inside_indices}{A numeric vector of the row indices (from \code{all_points_used}) of the points inside the ellipse.}
#'   \item{parameters}{A list of the key parameters used, including \code{level} and \code{method}.}
#' @details
#' This function provides two distinct statistical approaches for defining the
#' center and shape of the ellipse, selectable via the \code{method} parameter. The
#' size of the ellipse is controlled by the \code{level} parameter, which defines a
#' statistical confidence interval.
#'
#' ## Method
#'
#' The \code{method} argument determines how the centroid and covariance
#' matrix (shape and orientation) of the data cloud are calculated.
#'
#'  - "covmat" (Default): This is the classical approach, which uses the
#'     standard sample mean and sample covariance matrix calculated from all
#'     provided data points. While optimal for cleanly distributed, multivariate
#'     normal data, this method is highly sensitive to outliers. A single
#'     anomalous data point can significantly skew the mean and inflate the
#'     covariance matrix, resulting in an ellipse that poorly represents the
#'     central tendency of the data (Rousseeuw & Leroy, 2003).
#'
#'  - "mve": This option uses the Minimum Volume Ellipsoid (MVE) estimator,
#'     a robust statistical method designed to resist the influence of outliers
#'     (Van Aelst & Rousseeuw, 2009). Instead of using all data points, the MVE algorithm finds the
#'     ellipsoid with the smallest possible volume that contains a specified
#'     subset of the data (at least h = (n_points + n_variables + 1)/2 points)
#'     (Cobos et al., 2024). By focusing on the most concentrated "core" of the data,
#'     the MVE method effectively ignores outliers, providing a more reliable
#'     estimate of the data's true center and scatter when contamination is
#'     present (Van Aelst & Rousseeuw, 2009).
#'
#' ## Confidence Level
#'
#' The \code{level} parameter specifies the confidence level for the ellipse,
#' representing the percentage of the data that the ellipse is intended to
#' encompass(Cobos et al., 2024). It determines the size of the ellipse by defining a
#' statistical boundary based on Mahalanobis distances from the centroid.
#'
#' Assuming the data follows a multivariate normal distribution, the boundary of
#' the ellipse corresponds to a quantile of the chi-squared (\eqn{\chi^2})
#' distribution (Van Aelst & Rousseeuw, 2009). For example, a \code{level} of 95 (the default)
#' constructs an ellipse whose boundary is defined by the set of points having a
#' squared Mahalanobis distance equal to the 0.95 quantile of the \eqn{\chi^2}
#' distribution with 2 degrees of freedom (for a 2D analysis). Points with a
#' smaller Mahalanobis distance are inside the ellipse, while those with a
#' larger distance are outside.
#'
#' A higher \code{level} (e.g., 99) will result in a larger ellipse, while a lower
#' \code{level} (e.g., 90) will produce a smaller, more conservative ellipse.
#'
#' @references
#' Rousseeuw, P. J., & Leroy, A. M. (2003). Robust regression and outlier detection. John wiley & sons.
#'
#' Van Aelst, S., & Rousseeuw, P. (2009). Minimum volume ellipsoid. Wiley Interdisciplinary Reviews: Computational Statistics, 1(1), 71-82.
#'
#' Cobos, M.E., Osorio-Olvera, L., Soberón, J., Peterson, A.T., Barve, V. & Barve, N. (2024) ellipsenm: ecological niche’s characterizations using ellipsoids. <https://github.com/marlonecobos/ellipsenm>
#' @export
#' @importFrom stats cov var qchisq mahalanobis
#' @importFrom MASS cov.mve
#' @importFrom dplyr filter
#' @examples
#' \dontrun{
#' # 1. Create environmental data with a cluster and an outlier
#' set.seed(81)
#' env_data <- data.frame(
#'   BIO1 = c(rnorm(50, mean = 10, sd = 1), 30),
#'   BIO12 = c(rnorm(50, mean = 20, sd = 2), 50)
#' )
#'
#' # 2. Fit a 95% ellipse using the standard covariance method
#' fit <- fit_ellipsoid(
#'   data = env_data,
#'   env_vars = c("BIO1", "BIO12"),
#'   method = "covmat",
#'   level = 95
#' )
#'
#' # 3. Print the summary and plot the results
#' print(fit)
#' plot(fit)
#' }
fit_ellipsoid <- function(data, env_vars, method = "covmat", level = 95) {
  # --- Helper function to find the number of points for MVE ---
  ndata_quantile <- function(n_data, level) {
    n <- floor(n_data * level)
    return(min(n, n_data))
  }

  # --- Input Validation ---
  method <- match.arg(method, c("covmat", "mve"))

  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 100) {
    stop("`level` must be a single number greater than 0 and less than 100.")
  }
  if (length(env_vars) != 2 || !is.character(env_vars)) {
    stop("`env_vars` must be a character vector of length two.")
  }
  if (!all(env_vars %in% names(data))) {
    stop("Both variable names in `env_vars` must be present as columns in the data.")
  }

  clean_data <- data %>%
    dplyr::filter(is.finite(.data[[env_vars[1]]]) & is.finite(.data[[env_vars[2]]]))

  if (nrow(clean_data) < 3) {
    stop("At least 3 complete observations are needed to fit an ellipse.")
  }

  # Data is assumed to be pre-scaled
  env_data_matrix <- as.matrix(clean_data[, env_vars])
  level_prop <- level / 100

  # --- Centroid, Covariance, and Point Inclusion ---
  if (method == "covmat") {
    centroid <- colMeans(env_data_matrix)
    cov_mat <- stats::cov(env_data_matrix)
    mahal_dist_sq <- stats::mahalanobis(env_data_matrix, center = centroid, cov = cov_mat)
    threshold <- stats::qchisq(level_prop, df = 2)
    inside_indices <- which(mahal_dist_sq <= threshold)
  } else if (method == "mve") {
    n_quant <- ndata_quantile(nrow(env_data_matrix), level_prop)
    mve_res <- MASS::cov.mve(env_data_matrix, quantile.used = n_quant)
    centroid <- mve_res$center
    cov_mat <- mve_res$cov
    inside_indices <- mve_res$best
  }

  # --- Ellipse Polygon Generation ---
  n_points <- 100
  radius_plot <- sqrt(stats::qchisq(level_prop, df = 2))
  eigen_plot <- eigen(cov_mat)
  transformation_matrix <- eigen_plot$vectors %*% diag(sqrt(eigen_plot$values))
  angles <- seq(0, 2 * pi, length.out = n_points)
  unit_circle <- cbind(cos(angles), sin(angles))
  ellipse_points <- t(centroid + radius_plot * transformation_matrix %*% t(unit_circle))
  colnames(ellipse_points) <- env_vars

  # --- Construct Final S3 Object ---
  results <- list(
    niche_ellipse = as.data.frame(ellipse_points),
    centroid = centroid,
    covariance_matrix = cov_mat,
    all_points_used = clean_data,
    points_in_ellipse = clean_data[inside_indices, ],
    points_outside_ellipse = clean_data[setdiff(seq_len(nrow(clean_data)), inside_indices), ],
    inside_indices = inside_indices,
    parameters = list(level = level, method = method)
  )

  class(results) <- "bean_ellipsoid"
  return(results)
}


#' @export
#' @keywords internal
print.bean_ellipsoid <- function(x, ...) {
  cat("--- Bean Environmental Niche Ellipse ---\n\n")
  cat(sprintf("Method: '%s'.\n", x$parameters$method))
  cat(sprintf("Fitted to %d data points at a %.2f%% percentage of data.\n",
              nrow(x$all_points_used), x$parameters$level))
  cat(sprintf("%d out of %d points (%.1f%%) fall within the ellipse boundary.\n\n",
              nrow(x$points_in_ellipse), nrow(x$all_points_used),
              100 * nrow(x$points_in_ellipse) / nrow(x$all_points_used)))

  cat("Niche Centroid:\n")
  print(x$centroid)
  cat("\n")
}


#' @export
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_point geom_polygon labs theme_bw scale_color_manual .data
plot.bean_ellipsoid <- function(x, ...) {
  env_vars <- names(x$centroid)

  plot_data_all <- x$all_points_used
  ellipse_data <- x$niche_ellipse
  centroid_data <- data.frame(x = x$centroid[1], y = x$centroid[2])

  # Determine point status using the robust indices
  plot_data_all$bean_id <- seq_len(nrow(plot_data_all))
  plot_data_all$Status <- ifelse(plot_data_all$bean_id %in% x$inside_indices, "Inside", "Outside")

  # Create Plot
  fit_ellip_plot <- ggplot2::ggplot(plot_data_all, ggplot2::aes(x = .data[[env_vars[1]]], y = .data[[env_vars[2]]])) +
    ggplot2::geom_polygon(data = ellipse_data, fill = "#0072B2", alpha = 0.2) +
    ggplot2::geom_point(ggplot2::aes(color = Status), alpha = 0.7) +
    ggplot2::geom_point(
      data = centroid_data,
      ggplot2::aes(x = x, y = y),
      color = "red", size = 4, shape = 3, stroke = 1.5
    ) +
    ggplot2::scale_color_manual(name = "Point Status", values = c("Inside" = "darkgreen", "Outside" = "salmon")) +
    ggplot2::labs(
      title = "Fitted Environmental Niche Ellipse",
      subtitle = sprintf("Ellipse boundary defined by the '%s' method at %g%% level", x$parameters$method, x$parameters$level),
      x = env_vars[1],
      y = env_vars[2]
    ) +
    ggplot2::theme_bw()

  return(fit_ellip_plot)
}
