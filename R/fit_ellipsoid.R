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
#'   covariance matrix. Options are "covmat" (for a standard covariance matrix) and "mve"
#'   (Minimum Volume Ellipsoid, robust to outliers). Default = "covmat". See Details
#' @param level (numeric) A single value between 0 and 1 representing the
#'   proportion of data points the ellipse is intended to encompass.
#'   Default is 0.95.
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
#'     (Rousseeuw et al., 1984, 1985). Instead of using all data points, the MVE algorithm finds the
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
#' encompass (Cobos et al., 2024). It determines the size of the ellipse by defining a
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
#'
#' Rousseeuw, P. J. (1984). Least median of squares regression. Journal of the American statistical association, 79(388), 871-880.
#'
#' Rousseeuw, P. J. (1985). Multivariate estimation with high breakdown point. Mathematical statistics and applications, 8(283-297), 37.
#' @export
#' @importFrom stats cov var qchisq mahalanobis
#' @importFrom MASS cov.mve
#' @importFrom rgl ellipse3d
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
#'   level = 0.95
#' )
#'
#' # 3. Print the summary and plot the results
#' print(fit)
#' plot(fit)
#' }
fit_ellipsoid <- function(data, env_vars, method = "covmat", level = 0.95) {
  # --- Helper function for MVE ---
  ndata_quantile <- function(n_data, level_prop) {
    n <- floor(n_data * level_prop)
    return(min(n, n_data))
  }

  # --- Input Validation ---
  method <- match.arg(method, c("covmat", "mve"))

  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("`level` must be a single number greater than 0 and less than 1")
  }
  if (length(env_vars) < 2) {
    stop("`env_vars` must be a character vector of at least length two")
  }
  if (!all(env_vars %in% names(data))) {
    stop("One or more `env_vars` not found in the data frame")
  }

  clean_data <- data[stats::complete.cases(data[, env_vars]), ]

  if (nrow(clean_data) < (length(env_vars) + 1)) {
    stop("At least n_variables + 1 complete observations are needed to fit an ellipsoid")
  }

  env_data_matrix <- as.matrix(clean_data[, env_vars])
  n_dim <- length(env_vars)

  # --- Centroid, Covariance, and Point Inclusion ---
  if (method == "covmat") {
    centroid <- colMeans(env_data_matrix)
    cov_mat <- stats::cov(env_data_matrix)
    mahal_dist_sq <- stats::mahalanobis(env_data_matrix, center = centroid, cov = cov_mat)
    threshold <- stats::qchisq(level, df = n_dim)
    inside_indices <- which(mahal_dist_sq <= threshold)
  } else if (method == "mve") {
    n_quant <- ndata_quantile(nrow(env_data_matrix), level)
    mve_res <- MASS::cov.mve(env_data_matrix, quantile.used = n_quant)
    centroid <- mve_res$center
    cov_mat <- mve_res$cov
    inside_indices <- mve_res$best
  }

  # --- Ellipse Polygon/Mesh Generation ---
  ellipse_obj <- NULL
  if (n_dim == 2) {
    # Generate 2D polygon for ggplot
    n_points <- 100
    radius_plot <- sqrt(stats::qchisq(level, df = 2))
    eigen_plot <- eigen(cov_mat)
    transformation_matrix <- eigen_plot$vectors %*% diag(sqrt(eigen_plot$values))
    angles <- seq(0, 2 * pi, length.out = n_points)
    unit_circle <- cbind(cos(angles), sin(angles))
    ellipse_points_matrix <- t(centroid + radius_plot * transformation_matrix %*% t(unit_circle))
    colnames(ellipse_points_matrix) <- env_vars
    ellipse_obj <- as.data.frame(ellipse_points_matrix)
  } else if (n_dim >= 3) {
    # Generate 3D mesh for rgl
    if (requireNamespace("rgl", quietly = TRUE)) {
      vars_3d <- env_vars[1:3]
      cov_mat_3d <- cov_mat[vars_3d, vars_3d]
      centroid_3d <- centroid[vars_3d]
      # The t-statistic corresponds to the radius of the sphere before transformation
      radius_3d <- sqrt(stats::qchisq(level, df = 3))
      ellipse_obj <- rgl::ellipse3d(cov_mat_3d, centre = centroid_3d, t = radius_3d)
    }
  }

  # --- Construct Final S3 Object ---
  results <- list(
    niche_ellipse = ellipse_obj,
    centroid = centroid,
    covariance_matrix = cov_mat,
    all_points_used = as.data.frame(clean_data),
    points_in_ellipse = as.data.frame(clean_data[inside_indices, ]),
    points_outside_ellipse = as.data.frame(clean_data[setdiff(seq_len(nrow(clean_data)), inside_indices), ]),
    inside_indices = inside_indices,
    parameters = list(level = level, method = method)
  )

  class(results) <- "bean_ellipsoid"
  return(results)
}


#' @export
#' @keywords internal
print.bean_ellipsoid <- function(x, ...) {
  cat("--- Bean Environmental Niche Ellipsoid ---\n\n")
  cat(sprintf("Method: '%s'.\n", x$parameters$method))
  cat(sprintf("Fitted in %d dimensions to %d data points at a %.2f%% level.\n",
              length(x$centroid), nrow(x$all_points_used), x$parameters$level * 100))
  cat(sprintf("%d out of %d points (%.1f%%) fall within the ellipsoid boundary.\n\n",
              nrow(x$points_in_ellipse), nrow(x$all_points_used),
              100 * nrow(x$points_in_ellipse) / nrow(x$all_points_used)))

  cat("Niche Centroid:\n")
  print(x$centroid)
  cat("\n")
}


#' @export
#' @keywords internal
#' @importFrom grDevices adjustcolor
#' @importFrom rgl plot3d wire3d ellipse3d open3d bg3d
plot.bean_ellipsoid <- function(x, dims = c(1, 2), ..., window_size = c(800, 800)) {
  env_vars <- names(x$centroid)
  total_dim <- length(env_vars)

  # --- Input Validation for Dimensions ---
  if (is.numeric(dims)) {
    if (any(dims < 1 | dims > total_dim)) {
      stop("Numeric 'dims' indices are out of bounds.")
    }
    plot_vars <- env_vars[dims]
  } else if (is.character(dims)) {
    if (!all(dims %in% env_vars)) {
      stop("One or more 'dims' names not found in the ellipsoid variables.")
    }
    plot_vars <- dims
  } else {
    stop("'dims' must be a numeric vector of indices or a character vector of variable names.")
  }

  n_plot_dim <- length(plot_vars)

  # --- Define color palette ---
  colors <- list(
    inside = "#118ab2",       # Teal
    outside = "#ef476f",      # Coral/pink
    ellipse_fill = "#06d6a0", # Mint/aqua
    ellipse_line = "#073b4c", # Dark navy
    centroid = "#ffd166"      # Warm yellow
  )

  # --- Determine point status for coloring ---
  plot_data_all <- x$all_points_used
  point_status <- rep("Outside", nrow(plot_data_all))
  point_status[x$inside_indices] <- "Inside"

  # Base R needs an exact vector of colors
  point_colors <- ifelse(point_status == "Inside", colors$inside, colors$outside)

  # Use the chi-squared threshold from the full model to ensure correct boundaries
  level <- x$parameters$level
  radius_plot <- sqrt(stats::qchisq(level, df = total_dim))

  if (n_plot_dim == 2) {
    # ==========================================
    # 2D Plot (Base R Graphics)
    # ==========================================
    var1 <- plot_vars[1]
    var2 <- plot_vars[2]

    # Extract 2D sub-matrix and centroid
    centroid_2d <- x$centroid[plot_vars]
    cov_2d <- x$covariance_matrix[plot_vars, plot_vars]

    # Generate 2D ellipse polygon mathematically
    n_points <- 100
    eigen_plot <- eigen(cov_2d)
    transformation_matrix <- eigen_plot$vectors %*% diag(sqrt(eigen_plot$values))
    angles <- seq(0, 2 * pi, length.out = n_points)
    unit_circle <- cbind(cos(angles), sin(angles))
    ellipse_points <- t(centroid_2d + radius_plot * transformation_matrix %*% t(unit_circle))

    # Calculate plot limits to ensure everything fits
    x_range <- range(c(plot_data_all[[var1]], ellipse_points[, 1]), na.rm = TRUE)
    y_range <- range(c(plot_data_all[[var2]], ellipse_points[, 2]), na.rm = TRUE)

    # 1. Setup the empty plot canvas
    plot(x_range, y_range, type = "n",
         xlab = var1, ylab = var2,
         main = "Fitted Environmental Niche Ellipse",
         sub = sprintf("Method: '%s' | Confidence Level: %g%%", x$parameters$method, level * 100))

    # 2. Draw the shaded Ellipse Polygon
    polygon(ellipse_points[, 1], ellipse_points[, 2],
            col = grDevices::adjustcolor(colors$ellipse_fill, alpha.f = 0.3),
            border = colors$ellipse_line, lwd = 2)

    # 3. Draw the Occurrence Points
    points(plot_data_all[[var1]], plot_data_all[[var2]],
           col = grDevices::adjustcolor(point_colors, alpha.f = 0.8),
           pch = 16, cex = 1.2)

    # 4. Draw the Centroid
    points(centroid_2d[1], centroid_2d[2],
           col = colors$ellipse_line, bg = colors$centroid,
           pch = 22, cex = 1.8, lwd = 2)

    # 5. Add the Legend
    legend("topright",
           legend = c("Inside", "Outside", "Centroid"),
           col = c(colors$inside, colors$outside, colors$ellipse_line),
           pt.bg = c(NA, NA, colors$centroid),
           pch = c(16, 16, 23),
           bty = "n", pt.cex = 1.2, inset = 0.02)

    return(invisible(NULL))

  } else if (n_plot_dim >= 3) {
    # ==========================================
    # 3D Plot (rgl)
    # ==========================================
    if (!requireNamespace("rgl", quietly = TRUE)) {
      stop("Package 'rgl' is required for plotting ellipsoids with >= 3 dimensions. Please install it.", call. = FALSE)
    }

    vars_3d <- plot_vars[1:3]
    if (n_plot_dim > 3) {
      message(sprintf("Visualizing in 3D using the first three selected variables: %s, %s, %s",
                      vars_3d[1], vars_3d[2], vars_3d[3]))
    }

    # Extract 3D sub-matrix and centroid
    centroid_3d <- x$centroid[vars_3d]
    cov_3d <- x$covariance_matrix[vars_3d, vars_3d]

    # Generate 3D mesh dynamically
    mesh_3d <- rgl::ellipse3d(cov_3d, centre = centroid_3d, t = radius_plot)

    points_3d <- plot_data_all[, vars_3d]

    # Initialize RGL window
    rgl::open3d(windowRect = c(50, 50, 50 + window_size[1], 50 + window_size[2]))
    rgl::bg3d("#f7f7f7")

    # Determine a base radius for the points
    base_radius <- 0.01 * diff(range(points_3d, na.rm = TRUE))

    # Plot points as spheres
    rgl::plot3d(
      x = points_3d[[1]], y = points_3d[[2]], z = points_3d[[3]],
      col = point_colors,
      type = "s",
      radius = base_radius,
      xlab = vars_3d[1], ylab = vars_3d[2], zlab = vars_3d[3],
      aspect = "iso",
      lit = TRUE
    )

    # ---> ADDED: Plot the Centroid as a larger, distinct sphere <---
    rgl::spheres3d(
      x = centroid_3d[1], y = centroid_3d[2], z = centroid_3d[3],
      col = colors$centroid,
      radius = base_radius * 2.5, # Make the centroid 2.5x larger than normal points
      lit = TRUE
    )

    # Plot the ellipsoid wireframe
    rgl::wire3d(mesh_3d, col = colors$ellipse_line, alpha = 0.4, lit = FALSE)

    return(invisible(NULL))
  }
}
