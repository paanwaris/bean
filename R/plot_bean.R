#' Visualize environmental thinning results
#'
#' @description This function creates a comparison plot showing the original,
#' unthinned occurrence points overlaid with the results of an environmental
#' thinning process. It draws the environmental grid to visualize how points
#' were either sampled (stochastic) or consolidated to cell centers (deterministic).
#'
#' @param original_data A data.frame containing species occurrences and pre-scaled
#'   environmental variables, typically the output of \code{\link{prepare_bean}}.
#' @param thinned_object The output object from either \code{\link{thin_env_density}} or
#'   \code{\link{thin_env_center}}. The function will automatically detect the thinning type.
#' @param grid_resolution A numeric vector of length two specifying the grid
#'   resolution used for thinning. This is required to draw the grid correctly.
#' @param env_vars A character vector of length two specifying the names of the
#'   environmental variables to plot on the x and y axes.
#' @seealso \code{\link{prepare_bean}}, \code{\link{thin_env_density}}, \code{\link{thin_env_center}}
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_vline geom_hline labs theme_bw theme element_blank
#' @examples
#' \dontrun{
#' # Assume 'prepared_data' and 'grid_res' are available from previous steps.
#'
#' # --- Example 1: Visualizing Stochastic Thinning ---
#' set.seed(123)
#' thinned_stochastic <- thin_env_density(
#'   data = prepared_data,
#'   env_vars = c("BIO1", "BIO12"),
#'   grid_resolution = grid_res,
#'   max_per_cell = 1
#' )
#'
#' plot_bean(
#'   original_data = prepared_data,
#'   thinned_object = thinned_stochastic,
#'   grid_resolution = grid_res,
#'   env_vars = c("BIO1", "BIO12")
#' )
#'
#' # --- Example 2: Visualizing Deterministic Thinning ---
#' thinned_deterministic <- thin_env_center(
#'   data = prepared_data,
#'   env_vars = c("BIO1", "BIO12"),
#'   grid_resolution = grid_res
#' )
#'
#' plot_bean(
#'   original_data = prepared_data,
#'   thinned_object = thinned_deterministic,
#'   grid_resolution = grid_res,
#'   env_vars = c("BIO1", "BIO12")
#' )
#' }
plot_bean <- function(original_data, thinned_object, env_vars, grid_resolution) {

  # --- 1. Input Validation ---
  if (!inherits(thinned_object, c("bean_thinned_density", "bean_thinned_center"))) {
    stop("`thinned_object` must be an `bean_thinned_density` or `bean_thinned_center` object.")
  }
  if (length(grid_resolution) != 2) {
    stop("`grid_resolution` must be a numeric vector of length 2.")
  }
  if (!all(env_vars %in% names(original_data))) {
    stop("One or both `env_vars` not found in `original_data`.")
  }

  # --- 2. Calculate Grid Line Positions ---
  x_range <- range(original_data[[env_vars[1]]], na.rm = TRUE)
  x_breaks <- seq(
    from = floor(x_range[1] / grid_resolution[1]) * grid_resolution[1],
    to = ceiling(x_range[2] / grid_resolution[1]) * grid_resolution[1],
    by = grid_resolution[1]
  )

  y_range <- range(original_data[[env_vars[2]]], na.rm = TRUE)
  y_breaks <- seq(
    from = floor(y_range[1] / grid_resolution[2]) * grid_resolution[2],
    to = ceiling(y_range[2] / grid_resolution[2]) * grid_resolution[2],
    by = grid_resolution[2]
  )

  # --- 3. Create the Base Plot ---
  # This includes the original points and the grid lines
  base_plot <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = original_data,
      ggplot2::aes(x = .data[[env_vars[1]]], y = .data[[env_vars[2]]]),
      color = "grey60", alpha = 0.5, size = 2
    ) +
    ggplot2::geom_vline(xintercept = x_breaks, color = "grey80", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = y_breaks, color = "grey80", linetype = "dashed", linewidth = 0.5) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  # --- 4. Add Layers Based on Thinning Type ---
  if (inherits(thinned_object, "bean_thinned_density")) {
    # --- Stochastic Plot ---
    final_plot <- base_plot +
      ggplot2::geom_point(
        data = thinned_object$thinned_data,
        ggplot2::aes(x = .data[[env_vars[1]]], y = .data[[env_vars[2]]]),
        color = "#0072B2", alpha = 0.8, size = 1
      ) +
      ggplot2::labs(
        title = "Stochastic Thinning Comparison",
        subtitle = paste(thinned_object$n_thinned, "points remaining (blue) from", thinned_object$n_original, "original points (grey)"),
        x = paste(env_vars[1], "(scaled)"),
        y = paste(env_vars[2], "(scaled)")
      )
  } else {
    # --- Deterministic Plot ---
    final_plot <- base_plot +
      ggplot2::geom_point(
        data = thinned_object$thinned_points,
        ggplot2::aes(x = .data[[env_vars[1]]], y = .data[[env_vars[2]]]),
        color = "#D55E00", size = 2, shape = 3, stroke = 1
      ) +
      ggplot2::labs(
        title = "Deterministic Thinning to Grid Cell Centers",
        subtitle = paste(thinned_object$n_thinned, "unique cell centers (orange crosses) from", thinned_object$n_original, "original points (grey)"),
        x = paste(env_vars[1], "(scaled)"),
        y = paste(env_vars[2], "(scaled)")
      )
  }

  return(final_plot)
}
