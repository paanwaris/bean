#' Visualize n-dimensional environmental thinning results
#'
#' @description This function creates a pairs plot to visualize the results of
#'   n-dimensional environmental thinning. It can accept thinned objects from either
#'   density-based thinning (`thin_env_nd`) or deterministic centroid thinning
#'   (`thin_env_center`).
#'
#' @param original_data A data.frame of the prepared, unthinned occurrence points.
#' @param thinned_object The output object from `thin_env_nd()` or `thin_env_center()`.
#' @param env_vars A character vector of the environmental variables to plot.
#'
#' @return A `GGally::ggpairs` plot object.
#' @export
#' @importFrom stats setNames
#' @importFrom dplyr bind_rows mutate
#' @importFrom GGally ggpairs wrap
#' @importFrom ggplot2 ggplot aes theme_bw scale_color_manual scale_alpha_manual scale_shape_manual geom_vline geom_hline
#' @examples
#' \dontrun{
#' # Assume 'prepared_data' is ready.
#'
#' # --- Example: Plotting Density-Based Thinning ---
#' thinned_density_obj <- thin_env_nd(
#'   data = prepared_data,
#'   env_vars = c("PC1", "PC2", "PC3"),
#'   grid_resolution = 0.5,
#'   max_per_cell = 1
#' )
#'
#' plot_bean(
#'   original_data = prepared_data,
#'   thinned_object = thinned_density_obj,
#'   env_vars = c("PC1", "PC2", "PC3")
#' )
#' }
plot_bean <- function(original_data, thinned_object, env_vars) {
  # --- 1. Identify Thinning Type and Prepare Data ---
  if (inherits(thinned_object, "bean_thinned_density")) {
    thinned_df <- thinned_object$thinned_data
    plot_title <- "N-Dimensional Density Thinning"
  } else if (inherits(thinned_object, "bean_thinned_center")) {
    thinned_df <- thinned_object$thinned_points
    plot_title <- "N-Dimensional Centroid Thinning"
  } else {
    stop("`thinned_object` must be an output from thin_env_nd() or thin_env_center()")
  }

  if (!all(env_vars %in% names(original_data)) || !all(env_vars %in% names(thinned_df))) {
    stop("One or more `env_vars` not found in `original_data` or the thinned data")
  }

  # --- 2. Combine Data for Plotting ---
  plot_data <- dplyr::bind_rows(
    dplyr::mutate(original_data, Status = "Original"),
    dplyr::mutate(thinned_df, Status = "Thinned")
  )
  plot_data$Status <- factor(plot_data$Status, levels = c("Original", "Thinned"))

  # --- 3. Create the Base Pairs Plot ---
  pairs_plot <- GGally::ggpairs(
    plot_data,
    columns = env_vars,
    # CHANGE 1: Map 'shape' to Status instead of 'size'
    mapping = ggplot2::aes(color = Status, alpha = Status, shape = Status),
    title = plot_title,
    # CHANGE 2: Use a single, fixed size for all points
    upper = list(continuous = GGally::wrap("points")),
    lower = list(continuous = GGally::wrap("points")),
    diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.5))
  )

  # --- 4. Add Grid Lines and Scales to Each Panel ---
  resolutions_named <- stats::setNames(thinned_object$parameters$grid_resolution, env_vars)

  num_vars <- length(env_vars)
  for (i in 1:num_vars) {
    for (j in 1:num_vars) {
      if (i != j) {
        y_var <- env_vars[i]
        x_var <- env_vars[j]
        y_res <- resolutions_named[y_var]
        x_res <- resolutions_named[x_var]

        x_range <- range(original_data[[x_var]], na.rm = TRUE)
        y_range <- range(original_data[[y_var]], na.rm = TRUE)

        x_breaks <- seq(floor(x_range[1] / x_res) * x_res, ceiling(x_range[2] / x_res) * x_res, by = x_res)
        y_breaks <- seq(floor(y_range[1] / y_res) * y_res, ceiling(y_range[2] / y_res) * y_res, by = y_res)

        pairs_plot[i, j] <- pairs_plot[i, j] +
          ggplot2::geom_vline(xintercept = x_breaks, color = "grey70", linetype = "dotted", linewidth = 0.5) +
          ggplot2::geom_hline(yintercept = y_breaks, color = "grey70", linetype = "dotted", linewidth = 0.5) +
          ggplot2::scale_color_manual(values = c("Original" = "#ef476f", "Thinned" = "#0072B2")) +
          ggplot2::scale_alpha_manual(values = c("Original" = 0.2, "Thinned" = 0.6)) +
          ggplot2::scale_shape_manual(values = c("Original" = 19, "Thinned" = 1)) # Hollow vs. Solid circle
      }
    }
  }

  pairs_plot <- pairs_plot + ggplot2::theme_bw(base_size = 14)

  return(pairs_plot)
}
