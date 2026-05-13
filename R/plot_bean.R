#' Visualize n-dimensional environmental thinning results
#'
#' @description This function creates a scatterplot matrix (pairs plot) to
#'   visualize the results of n-dimensional environmental thinning using base R
#'   graphics. It can accept thinned objects from either density-based thinning
#'   (`thin_env_nd`) or deterministic centroid thinning (`thin_env_center`).
#'
#' @param original_data A data.frame of the prepared, unthinned occurrence points.
#' @param thinned_object The output object from `thin_env_nd()` or `thin_env_center()`.
#' @param env_vars A character vector of the environmental variables to plot.
#'
#' @return Invisibly returns `NULL`. Draws a plot to the active graphics device.
#' @export
#' @importFrom stats setNames density
#' @importFrom grDevices adjustcolor
#' @importFrom graphics par plot polygon text box axis abline points title legend
#' @examples
#' \dontrun{
#' # Assume 'prepared_data' is ready.
#'
#' # --- Example: Plotting Density-Based Thinning ---
#' thinned_obj <- thin_env_nd(
#'   data = prepared_data,
#'   env_vars = c("PC1", "PC2", "PC3"),
#'   grid_resolution = c(0.5, 0.5, 0.5)
#' )
#'
#' plot_bean(
#'   original_data = prepared_data,
#'   thinned_object = thinned_obj,
#'   env_vars = c("PC1", "PC2", "PC3")
#' )
#' }
plot_bean <- function(original_data, thinned_object, env_vars) {

  # --- 1. Identify Thinning Type and Prepare Data ---
  if (inherits(thinned_object, "bean_thinned")) {
    thinned_df <- thinned_object$thinned_data
    plot_title <- "N-Dimensional Density Thinning"
  } else if (inherits(thinned_object, "bean_thinned_center")) {
    thinned_df <- thinned_object$thinned_points
    plot_title <- "N-Dimensional Centroid Thinning"
  } else {
    stop("`thinned_object` must be an output from thin_env_nd or thin_env_center function")
  }

  if (!all(env_vars %in% names(original_data)) || !all(env_vars %in% names(thinned_df))) {
    stop("One or more `env_vars` not found in `original_data` or the thinned data")
  }

  # Match resolutions to variable names
  resolutions_named <- stats::setNames(thinned_object$parameters$grid_resolution, env_vars)

  num_vars <- length(env_vars)

  # --- 2. Setup Base R Plot Matrix Layout ---
  # Save the user's original graphics parameters so we can restore them later
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  # Create a grid.
  # mar leaves a small gap between panels, oma leaves room for the outer axes and right legend
  graphics::par(mfrow = c(num_vars, num_vars),
                mar = c(2, 2, 0.5, 0.5),
                oma = c(2, 2, 4, 10))

  # Define standard aesthetic colors
  col_orig <- grDevices::adjustcolor("#ef476f", alpha.f = 0.2)
  col_thin <- grDevices::adjustcolor("#0072B2", alpha.f = 0.8)
  col_orig_dens <- grDevices::adjustcolor("#ef476f", alpha.f = 0.3)
  col_thin_dens <- grDevices::adjustcolor("#0072B2", alpha.f = 0.3)

  # --- 3. Loop through combinations to build the Pairs Matrix ---
  for (i in 1:num_vars) {
    for (j in 1:num_vars) {
      y_var <- env_vars[i]
      x_var <- env_vars[j]

      # Lock axes ranges so all plots align perfectly
      x_range <- range(original_data[[x_var]], na.rm = TRUE)
      y_range <- range(original_data[[y_var]], na.rm = TRUE)

      if (i == j) {
        # ==========================================
        # Diagonal: Overlaid Density Plots
        # ==========================================
        d_orig <- stats::density(original_data[[x_var]], na.rm = TRUE)
        d_thin <- stats::density(thinned_df[[x_var]], na.rm = TRUE)

        graphics::plot(0, 0, type = "n",
                       xlim = x_range, ylim = range(c(0, d_orig$y, d_thin$y)),
                       axes = FALSE, xlab = "", ylab = "")

        # Draw shaded density curves
        graphics::polygon(d_orig, col = col_orig_dens, border = "#ef476f", lwd = 1.5)
        graphics::polygon(d_thin, col = col_thin_dens, border = "#0072B2", lwd = 1.5)

        # Print the variable name in the center
        graphics::text(x = mean(x_range), y = max(c(d_orig$y, d_thin$y)) * 0.85,
                       labels = x_var, font = 2, cex = 1.5)

        graphics::box(col = "grey50")

        # Only draw X-axis if it's on the bottom row
        if (i == num_vars) {
          graphics::axis(1, col = "grey50", col.axis = "grey30", cex.axis = 0.9)
        }

      } else {
        # ==========================================
        # Off-Diagonal: Scatterplots
        # ==========================================
        graphics::plot(0, 0, type = "n", xlim = x_range, ylim = y_range,
                       axes = FALSE, xlab = "", ylab = "")

        # 1. Add Grid lines based on calculated environmental resolution
        x_res <- resolutions_named[x_var]
        y_res <- resolutions_named[y_var]

        x_breaks <- seq(floor(x_range[1] / x_res) * x_res, ceiling(x_range[2] / x_res) * x_res, by = x_res)
        y_breaks <- seq(floor(y_range[1] / y_res) * y_res, ceiling(y_range[2] / y_res) * y_res, by = y_res)

        graphics::abline(v = x_breaks, col = "grey80", lty = 3)
        graphics::abline(h = y_breaks, col = "grey80", lty = 3)

        # 2. Add Original Points (Background)
        graphics::points(original_data[[x_var]], original_data[[y_var]],
                         col = col_orig, pch = 16, cex = 1.2)

        # 3. Add Thinned Points (Foreground)
        graphics::points(thinned_df[[x_var]], thinned_df[[y_var]],
                         col = col_thin, pch = 1, lwd = 2, cex = 1.2)

        graphics::box(col = "grey50")

        # 4. Handle Axis Labels
        if (i == num_vars) {
          graphics::axis(1, col = "grey50", col.axis = "grey30", cex.axis = 0.9)
        }
        if (j == 1) {
          graphics::axis(2, col = "grey50", col.axis = "grey30", cex.axis = 0.9, las = 1)
        }
      }
    }
  }

  # --- 4. Add Main Title and Master Legend ---
  graphics::title(main = plot_title, outer = TRUE, line = 1, cex.main = 1.8, font.main = 2)

  # Create an invisible top-level plot layer to safely place the legend outside the matrix
  graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  graphics::legend("right", inset = c(0.01, 0), title = "Data Status",
                   legend = c("Original", "Thinned"),
                   col = c("#ef476f", "#0072B2"),
                   pch = c(16, 1), pt.lwd = c(1, 2), pt.cex = 1.5,
                   bty = "n", cex = 1.2, text.font = 2)

  return(invisible(NULL))
}
