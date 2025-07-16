#' Fit a bivariate ellipse to environmental data
#'
#' @description This function calculates the bivariate ellipse that encompasses
#' a specified proportion of the data points in a 2D environmental space. It also
#' identifies which of the input points fall within the calculated ellipse.
#'
#' @param data A data frame containing species occurrences and environmental data.
#'   It is highly recommended that this data be scaled.
#' @param env_vars A character vector of length two specifying the names of the
#'   environmental variables to use.
#' @param level The confidence level (e.g., 0.95 for 95%) for the prediction
#'   ellipse. This determines what proportion of the points the ellipse should
#'   contain. Default = 0.95.
#' @param n_points The number of points to use for drawing the ellipse polygon.
#'   Default = 100.
#'
#' @return An object of class \code{bean_ellipsoid}.
#'
#' @export
#' @importFrom stats cov var qchisq mahalanobis
fit_ellipsoid <- function(data, env_vars, level = 0.95, n_points = 100) {
  # --- Input Validation and Data Cleaning ---
  if (!all(env_vars %in% names(data))) {
    stop("One or both specified env_vars not found in the data frame.")
  }

  env_var1_sym <- rlang::sym(env_vars[1])
  env_var2_sym <- rlang::sym(env_vars[2])

  clean_data <- data %>%
    dplyr::filter(is.finite(!!env_var1_sym) & is.finite(!!env_var2_sym))

  if (nrow(clean_data) < 3) {
    stop("At least 3 complete observations are needed to fit an ellipse.")
  }

  env_data <- clean_data[, env_vars]

  # --- Ellipse Calculation ---
  mu <- colMeans(env_data)
  sigma <- stats::cov(env_data)

  # Generate a circle of points
  radius <- sqrt(stats::qchisq(level, df = 2))
  angles <- seq(0, 2 * pi, length.out = n_points)
  unit_circle <- cbind(cos(angles), sin(angles))

  # Transform the circle into an ellipse
  eigen_decomp <- eigen(sigma)
  transformation_matrix <- eigen_decomp$vectors %*% diag(sqrt(eigen_decomp$values))

  ellipse_points <- t(mu + radius * transformation_matrix %*% t(unit_circle))
  colnames(ellipse_points) <- env_vars

  # --- Identify Points Inside the Ellipse ---
  # Calculate Mahalanobis distance (squared) for each point
  mahal_dist_sq <- stats::mahalanobis(env_data, center = mu, cov = sigma)

  # The threshold is the chi-squared value for the specified level
  threshold <- stats::qchisq(level, df = 2)

  # Filter the original data to get points inside the ellipse
  points_inside <- clean_data[mahal_dist_sq <= threshold, ]

  # --- Construct S3 Object ---
  results <- list(
    niche_ellipse = as.data.frame(ellipse_points),
    niche_center = mu,
    covariance_matrix = sigma,
    all_points_used = clean_data,
    points_in_ellipse = points_inside,
    level = level
  )

  class(results) <- "bean_ellipsoid"
  return(results)
}

#' @export
print.bean_ellipsoid <- function(x, ...) {
  cat("--- Bean Environmental Niche Ellipse ---\n\n")
  cat(sprintf("Fitted to %d data points at a %.2f%% confidence level.\n",
              nrow(x$all_points_used), x$level * 100))
  cat(sprintf("%d out of %d points (%.1f%%) fall within the ellipse boundary.\n\n",
              nrow(x$points_in_ellipse), nrow(x$all_points_used),
              100 * nrow(x$points_in_ellipse) / nrow(x$all_points_used)))

  cat("Niche Center (Mean Vector):\n")
  print(x$niche_center)
  cat("\n")
}
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_polygon labs theme_bw scale_color_manual
plot.bean_ellipsoid <- function(x, ...) {
  # Get the names of the environmental variables from the raw points
  env_vars <- colnames(x$all_points_used)[colnames(x$all_points_used) %in% names(x$niche_center)]

  # --- Corrected Logic for Labeling Points ---
  # Create a stable, unique ID for each point in both data frames
  # This assumes the original data has columns like 'decimalLongitude' and 'decimalLatitude'
  # If not, any unique identifier per row would work.
  # For this example, we'll create one if it doesn't exist.

  plot_data <- x$all_points_used
  if (!"unique_id" %in% names(plot_data)) {
    plot_data$unique_id <- 1:nrow(plot_data)
  }

  points_inside <- x$points_in_ellipse
  if (!"unique_id" %in% names(points_inside)) {
    points_inside$unique_id <- 1:nrow(points_inside)
  }

  # Create the 'Status' column based on the stable unique ID
  plot_data$Status <- ifelse(plot_data$unique_id %in% points_inside$unique_id,
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
      subtitle = sprintf("Ellipse contains %.0f%% of the core environmental niche", x$level * 100),
      x = paste("Scaled", env_vars[1]),
      y = paste("Scaled", env_vars[2])
    ) +
    ggplot2::theme_bw()
}
