#' Fit a bivariate environmental ellipsoid to occurrence data
#'
#' @description This function calculates a bivariate ellipse that encompasses a
#' specified proportion of the data points in a 2D environmental space. It can
#' use either a standard covariance matrix or a robust Minimum Volume Ellipsoid.
#' The function also correctly identifies which of the input points fall within
#' and outside the calculated ellipse boundary, preserving all original columns.
#'
#' @param data A data frame containing species occurrences and environmental data.
#'   It is highly recommended that this data be scaled.
#' @param var1 (character) The name of the first environmental variable column.
#' @param var2 (character) The name of the second environmental variable column.
#' @param method (character) The method for calculating the centroid and
#'   covariance matrix. Options are "covmat" (standard covariance) and "mve"
#'   (Minimum Volume Ellipsoid, robust to outliers). Default = "covmat".
#' @param level (numeric) The confidence level (e.g., 95 for 95%) for the prediction
#'   ellipse. This determines what proportion of the points the ellipse should
#'   contain. Default = 95.
#'
#' @return An object of class \code{bean_ellipsoid}.
#'
#' @export
#' @importFrom stats cov var qchisq mahalanobis
#' @importFrom MASS cov.mve
#' @examples
#' \dontrun{
#' # 1. Create some thinned data to work with
#' set.seed(81)
#' thinned_data <- data.frame(
#'   BIO1 = rnorm(50),
#'   BIO12 = rnorm(50)
#' )
#'
#' # 2. Fit a 95% ellipse using the robust covariance matrix method
#' niche_ellipse <- fit_ellipsoid(
#'   data = thinned_data,
#'   var1 = "BIO1",
#'   var2 = "BIO12",
#'   method = "covmat",
#'   level = 95
#' )
#'
#' # 3. Print the summary and plot the result
#' print(niche_ellipse)
#' plot(niche_ellipse)
#' }
fit_ellipsoid <- function(data, var1, var2, method = "covmat", level = 95) {
  # --- Helper function to find the number of points for MVE ---
  ndata_quantile <- function(n_data, level) {
    n <- floor(n_data * level)
    return(min(n, n_data))
  }

  # --- Input Validation and Data Cleaning ---
  if (!all(c(var1, var2) %in% names(data))) {
    stop("Both var1 and var2 must be present as column names in the data.")
  }

  env_vars <- c(var1, var2)

  # Filter for finite rows but keep all original columns
  clean_data <- data %>%
    dplyr::filter(is.finite(.data[[var1]]) & is.finite(.data[[var2]]))

  if (nrow(clean_data) < 3) {
    stop("At least 3 complete observations are needed to fit an ellipse.")
  }

  # Create a matrix with only the env variables for calculations
  env_data_matrix <- as.matrix(clean_data[, env_vars])
  level_prop <- level / 100 # Convert percentage to proportion

  # --- Centroid, Covariance, and Point Inclusion ---
  if (method == "covmat") {
    centroid <- colMeans(env_data_matrix)
    cov_mat <- stats::cov(env_data_matrix)

    # Identify points inside using Mahalanobis distance
    mahal_dist_sq <- stats::mahalanobis(env_data_matrix, center = centroid, cov = cov_mat)
    threshold <- stats::qchisq(level_prop, df = 2)
    inside_indices <- which(mahal_dist_sq <= threshold)

  } else if (method == "mve") {
    n_quant <- ndata_quantile(nrow(env_data_matrix), level_prop)
    mve_res <- MASS::cov.mve(env_data_matrix, quantile.used = n_quant)
    centroid <- mve_res$center
    cov_mat <- mve_res$cov

    # Identify points inside using the 'best' subset from the MVE object
    inside_indices <- mve_res$best

  } else {
    stop("Invalid method. Choose 'covmat' or 'mve'.")
  }

  # Subset the full clean_data data frame to preserve all columns
  points_inside <- clean_data[inside_indices, ]
  points_outside <- clean_data[-inside_indices, ]

  # --- Ellipse Polygon Generation ---
  n_points <- 100 # Hardcoded for smooth plotting
  radius_plot <- sqrt(stats::qchisq(level_prop, df = 2))
  eigen_plot <- eigen(cov_mat)
  transformation_matrix <- eigen_plot$vectors %*% diag(sqrt(eigen_plot$values))
  angles <- seq(0, 2 * pi, length.out = n_points)
  unit_circle <- cbind(cos(angles), sin(angles))
  ellipse_points <- t(centroid + radius_plot * transformation_matrix %*% t(unit_circle))
  colnames(ellipse_points) <- env_vars

  # --- Construct S3 Object ---
  results <- list(
    centroid = centroid,
    covariance_matrix = cov_mat,
    level = level,
    method = method,
    niche_ellipse = as.data.frame(ellipse_points),
    all_points_used = clean_data,
    points_in_ellipse = points_inside,
    points_outside_ellipse = points_outside # New element
  )

  class(results) <- "bean_ellipsoid"
  return(results)
}

#' @export
print.bean_ellipsoid <- function(x, ...) {
  cat("--- Bean Environmental Niche Ellipse ---\n\n")
  cat(sprintf("Fitted to %d data points at a %.2f%% confidence level.\n",
              nrow(x$all_points_used), x$level))
  cat(sprintf("%d out of %d points (%.1f%%) fall within the ellipse boundary.\n\n",
              nrow(x$points_in_ellipse), nrow(x$all_points_used),
              100 * nrow(x$points_in_ellipse) / nrow(x$all_points_used)))
  cat("Niche Centroid (Mean Vector):\n")
  print(x$centroid)
  cat("\n")
}
#' Plot bean_ellipsoid results
#'
#' Creates a diagnostic plot from the output of \code{\link{fit_ellipsoid}}.
#'
#' @param x An object of class \code{bean_ellipsoid}.
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_polygon labs theme_bw scale_color_manual
plot.bean_ellipsoid <- function(x, ...) {
  # --- Corrected and more robust way to get env_vars ---
  env_vars <- colnames(x$niche_ellipse)

  if(is.null(env_vars) || length(env_vars) != 2) {
    stop("Could not determine environmental variables from the result object.")
  }

  # --- Create a stable unique ID for labeling points ---
  plot_data <- x$all_points_used
  plot_data$stable_id <- paste(plot_data[[env_vars[1]]], plot_data[[env_vars[2]]], sep = "_")

  points_inside <- x$points_in_ellipse
  points_inside$stable_id <- paste(points_inside[[env_vars[1]]], points_inside[[env_vars[2]]], sep = "_")

  # Create the 'Status' column based on the stable unique ID
  plot_data$Status <- ifelse(plot_data$stable_id %in% points_inside$stable_id,
                             "Inside", "Outside")

  fit_ellip_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[env_vars[1]]], y = .data[[env_vars[2]]])) +
    # Draw the ellipse polygon first, as a background
    ggplot2::geom_polygon(data = x$niche_ellipse, fill = "#0072B2", alpha = 0.2) +
    # Draw all points, colored by status
    ggplot2::geom_point(ggplot2::aes(color = Status), alpha = 0.7) +
    # Add a point for the niche center
    ggplot2::geom_point(
      data = data.frame(x = x$centroid[1], y = x$centroid[2]),
      ggplot2::aes(x = x, y = y),
      color = "red",
      size = 4,
      shape = 3,
      stroke = 1.5
    ) +
    ggplot2::scale_color_manual(name = "Point Status", values = c("Inside" = "darkgreen", "Outside" = "salmon")) +
    ggplot2::labs(
      title = "Fitted Environmental Niche Ellipse",
      subtitle = sprintf("Ellipse boundary defined by the '%s' method at %g%% level", x$method, x$level),
      x = paste(env_vars[1]),
      y = paste(env_vars[2])
    ) +
    ggplot2::theme_bw()

  return(fit_ellip_plot)
}
