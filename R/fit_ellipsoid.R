#' Fit a bivariate environmental ellipsoid to occurrence data
#'
#' @param data A data frame containing scaled environmental values and coordinates.
#' @param var1 The name of the first environmental variable (e.g., "BIO1").
#' @param var2 The name of the second environmental variable (e.g., "BIO12").
#' @param method Method used to estimate the ellipse: "covmat", "mve1", or "mve2".
#' @param level Confidence level (e.g., 95).
#'
#' @return An object of class \code{bean_ellipsoid}.
#' @export
fit_ellipsoid <- function(data, var1, var2, method = "covmat", level = 95) {
  if (!all(c(var1, var2) %in% names(data))) {
    stop("Both var1 and var2 must be present in the data.")
  }

  level <- level / 100
  env_data <- na.omit(data[, c(var1, var2)])

  # --- Centroid and Covariance Matrix ---
  if (method == "covmat") {
    centroid <- colMeans(env_data)
    cov_mat <- stats::cov(env_data)
  } else if (method == "mve1") {
    n <- floor(nrow(env_data) * level)
    mve <- MASS::cov.mve(env_data, quantile.used = n)
    centroid <- mve$center
    cov_mat <- mve$cov
  } else if (method == "mve2") {
    mvee <- mbased_mve(env_data, fitting_tolerance = 0.001)
    centroid <- mvee[[1]]
    cov_mat <- mvee[[2]]
  } else {
    stop("Invalid method. Use 'covmat', 'mve1', or 'mve2'.")
  }

  # --- Ellipse Construction ---
  radius <- sqrt(stats::qchisq(level, df = 2))
  angles <- seq(0, 2 * pi, length.out = 100)
  unit_circle <- cbind(cos(angles), sin(angles))

  eig <- eigen(cov_mat)
  transform_mat <- eig$vectors %*% diag(sqrt(eig$values))
  ellipse_coords <- t(centroid + radius * transform_mat %*% t(unit_circle))
  colnames(ellipse_coords) <- c(var1, var2)

  # --- Mahalanobis Distance ---
  md_sq <- mahalanobis(env_data, center = centroid, cov = cov_mat)
  threshold <- stats::qchisq(level, df = 2)
  inside_idx <- which(md_sq <= threshold)
  inside_points <- env_data[inside_idx, ]

  # --- Return S3 Object ---
  result <- list(
    niche_ellipse = as.data.frame(ellipse_coords),
    niche_center = centroid,
    covariance_matrix = cov_mat,
    all_points_used = env_data,
    points_in_ellipse = inside_points,
    level = level
  )
  class(result) <- "bean_ellipsoid"
  return(result)
}

#' @export
print.bean_ellipsoid <- function(x, ...) {
  cat("--- Bean Environmental Niche Ellipse ---\n\n")
  cat(sprintf("Fitted to %d data points at a %.2f%% confidence level.\n",
              nrow(x$all_points_used), x$level * 100))
  cat(sprintf("%d out of %d points (%.1f%%) fall within the ellipse boundary.\n\n",
              nrow(x$points_in_ellipse), nrow(x$all_points_used),
              100 * nrow(x$points_in_ellipse) / nrow(x$all_points_used)))
  cat("Niche Centroid (Mean Vector):\n")
  print(x$niche_center)
  cat("\n")
}

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

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[env_vars[1]]], y = .data[[env_vars[2]]])) +
    # Draw the ellipse polygon first, as a background
    ggplot2::geom_polygon(data = x$niche_ellipse, fill = "#0072B2", alpha = 0.2) +
    # Draw all points, colored by status
    ggplot2::geom_point(ggplot2::aes(color = Status), alpha = 0.7) +
    # Add a point for the niche center
    ggplot2::geom_point(
      data = data.frame(x = x$niche_center[1], y = x$niche_center[2]),
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
}
