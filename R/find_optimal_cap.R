#' Find optimal density caps based on a target thinning percentage
#'
#' @description This function searches for an optimal density cap by evaluating two criteria:
#' 1) the density cap that results in an occurrence point count closest
#'    to the target percentage, and
#' 2) the density cap that results in an occurrence point count that is closest
#'    to, but not below, the target percentage.
#'
#' @param data A data.frame containing species occurrence coordinates and the environmental variables.
#' @param env_vars A character vector specifying the column names in data that represent the environmental variables to be used in the analysis.
#' @param grid_resolution A numeric vector of length one or two specifying the
#'   resolution(s) for the grid axes. If length one, it is used for both axes. See Details
#' @param target_percent A numeric value (0-1) for the target percentage of
#'   points to retain. A value of 0.95 is recommended to remove 5 percent of the most densely clustered points while retaining most of the data. Default = 0.95.
#' @details
#' ### Defining Environmental Grid Axes
#'
#' The "grid_resolution" parameter is fundamental to how this function operates.
#' It defines the dimensions of the grid cells in the environmental space,
#' which are used to calculate occurrence density.
#'
#' A single numeric value (e.g., "grid_resolution = 0.2") will create
#'     square grid cells, applying the same resolution to both environmental
#'     axes.
#' A numeric vector of two values (e.g., "grid_resolution = c(0.2, 0.5)")
#'     will create rectangular grid cells, applying a different resolution
#'     to each respective axis.
#'
#' This function's flexibility allows you to tailor the thinning process to the specific
#' characteristics of your environmental data.
#'
#' @note It is highly recommended to first use the \code{\link{find_env_resolution}}
#' function to determine an objective, data-driven resolution. This allows you
#' to tailor the thinning process to the specific characteristics of your
#' environmental data.
#'
#' @seealso \code{\link{find_env_resolution}}
#'
#' @return An object of class \code{bean_optimization}, which is a list containing:
#'   \item{best_cap_closest}{The integer density cap value that results in a final count of occurrence points nearest to the target count.}
#'   \item{retained_points_closest}{The number of occurrence points retained after thinning with the density cap that provides the closest result.}
#'   \item{best_cap_above_target}{The smallest integer density cap value that retains a number of occurrence points greater than or equal to the target occurrence point count. This is often the most practical value to use.}
#'   \item{retained_points_above_target}{The number of occurrence points retained after thinning with the best density cap that is at or above the target.}
#'   \item{search_results}{A data.frame (as a tibble) containing the thinning results for every density cap value tested.}
#'   \item{parameters}{A list of key input parameters used in the optimization, such as the calculated target occurrence point count.}
#'
#' @export
#' @importFrom dplyr mutate count pull tibble add_row slice_min filter
#' @importFrom rlang sym
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @examples
#' \dontrun{
#' # 1. Create the example occurrence records and environmental variables
#' occ_data <- data.frame(
#'  BIO1 = c(0, 1, 3, 6),
#'  BIO12 = c(0, 10, 30, 60),
#'  x = c(1, 1, 0, 0.5),
#'  y = c(0, 0.5, 2, 1.5),
#'  species = "A"
#' )
#'
#' # 2. Find optimal cap to retain ~95% of the data
#' set.seed(81) # For reproducibility
#' optimal_params <- find_optimal_cap(
#'   data = occ_data,
#'   env_vars = c("BIO1", "BIO12"),
#'   grid_resolution = c(0.1, 0.2), # Using an example resolution
#'   target_percent = 0.95
#' )
#'
#' # 3. Print the summary and plot the results
#' print(optimal_params)
#' plot(optimal_params)
#' }
find_optimal_cap <- function(data, env_vars, grid_resolution, target_percent = 0.95) {
  # --- Input Validation ---
  if (length(env_vars) < 2) {
    stop("`env_vars` must contain at least two variable names")
  }
  if (length(env_vars) != length(grid_resolution)) {
    stop("Length of `grid_resolution` must match the length of `env_vars`")
  }
  if (!all(env_vars %in% names(data))) {
    stop("One or more `env_vars` not found in the data frame")
  }

  # --- Data Cleaning ---
  clean_data <- data[stats::complete.cases(data[, env_vars]), ]

  if (nrow(clean_data) == 0) {
    stop("No complete observations to run optimization.")
  }

  # --- Begin Optimization ---
  target_point_count <- floor(nrow(clean_data) * target_percent)

  # Dynamic N-Dimensional Grid Cell ID Creation
  env_data <- clean_data[, env_vars, drop = FALSE]
  gridded_vals <- t(t(env_data) / grid_resolution)
  cell_ids <- apply(floor(gridded_vals), 1, paste, collapse = "_")

  max_density <- table(cell_ids) %>% max()

  if (!is.finite(max_density) || max_density < 1) {
    message("Could not determine a valid maximum density. No thinning will be performed.")
    max_density <- 1
  }

  cap_candidates <- 1:max_density
  search_results <- dplyr::tibble(cap = integer(), thinned_count = integer())

  cat("Searching for optimal cap...\n")
  pb <- utils::txtProgressBar(min = 0, max = max_density, style = 3)

  for (cap in cap_candidates) {
    # Use the n-dimensional thinning function
    thinned_data <- thin_env_nd(
      data = clean_data,
      env_vars = env_vars,
      grid_resolution = grid_resolution,
      max_per_cell = cap
    )$thinned_data

    search_results <- search_results %>%
      dplyr::add_row(cap = cap, thinned_count = nrow(thinned_data))

    utils::setTxtProgressBar(pb, cap)
  }
  close(pb)

  # --- Process Results ---
  best_result_closest <- search_results %>%
    dplyr::mutate(difference = abs(thinned_count - target_point_count)) %>%
    dplyr::slice_min(order_by = difference, n = 1, with_ties = TRUE) %>%
    dplyr::slice_min(order_by = cap, n = 1)

  best_result_above_target <- search_results %>%
    dplyr::filter(thinned_count >= target_point_count) %>%
    dplyr::slice_min(order_by = thinned_count, n = 1, with_ties = TRUE) %>%
    dplyr::slice_min(order_by = cap, n = 1, with_ties = FALSE)

  if (nrow(best_result_above_target) == 0) {
    best_cap_above_target_val <- NA
    retained_points_above_target <- NA
  } else {
    best_cap_above_target_val <- best_result_above_target$cap
    retained_points_above_target <- best_result_above_target$thinned_count
  }

  results <- list(
    best_cap_closest = best_result_closest$cap,
    retained_points_closest = best_result_closest$thinned_count,
    best_cap_above_target = best_cap_above_target_val,
    retained_points_above_target = retained_points_above_target,
    search_results = search_results,
    parameters = list(
      target_point_count = target_point_count
    )
  )

  class(results) <- "bean_optimization"
  return(results)
}

#' @export
#' @keywords internal
print.bean_optimization <- function(x, ...) {
  cat("--- Bean Optimization Results ---\n\n")
  cat(sprintf("Target: Retain >= %d occurrence points.\n\n", x$parameters$target_point_count))

  cat("Recommendation for 'Closest to Target':\n")
  cat(sprintf("  - Best Cap: %d\n", x$best_cap_closest))
  cat(sprintf("  - Retained Points: %d (Difference of %d)\n\n",
              x$retained_points_closest,
              abs(x$retained_points_closest - x$parameters$target_point_count)))

  cat("Recommendation for 'Closest Above Target' (Recommended for use):\n")
  if (is.na(x$best_cap_above_target)) {
    cat("  - No cap found that was at or above the target count.\n")
  } else {
    cat(sprintf("  - Best Cap: %d\n", x$best_cap_above_target))
    cat(sprintf("  - Retained Points: %d\n", x$retained_points_above_target))
  }

  cat("\n---------------------------------\n")
  invisible(x)
}

#' @export
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_vline geom_hline labs theme_bw scale_y_continuous
#' @importFrom scales comma
plot.bean_optimization <- function(x, ...) {
  cap_plot <- ggplot2::ggplot(x$search_results, ggplot2::aes(x = cap, y = thinned_count)) +
    ggplot2::geom_line(color = "gray50", alpha = 0.8) +
    ggplot2::geom_point(color = "black", size = 2) +
    ggplot2::geom_hline(
      yintercept = x$parameters$target_point_count,
      linetype = "dashed", color = "#E41A1C", linewidth = 1
    ) +
    ggplot2::geom_vline(
      xintercept = x$best_cap_closest,
      linetype = "dashed", color = "#377EB8", linewidth = 1
    )

  if (!is.na(x$best_cap_above_target)) {
    cap_plot <- cap_plot +
      ggplot2::geom_vline(
        xintercept = x$best_cap_above_target,
        linetype = "dashed", color = "#4DAF4A", linewidth = 1
      )
  }

  caption_text <- paste0(
    "Red line: Target count (", x$parameters$target_point_count, ")\n",
    "Blue line: 'Closest' cap (", x$best_cap_closest, ")"
  )
  if (!is.na(x$best_cap_above_target)) {
    caption_text <- paste0(caption_text, "\nGreen line: 'Above Target' cap (", x$best_cap_above_target, ")")
  }

  cap_plot <- cap_plot +
    ggplot2::labs(
      title = "Search for Optimal Density Cap",
      x = "Maximum Points per Cell (Cap)",
      y = "Number of Points Retained",
      caption = caption_text
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme_bw(base_size = 12)

  return(cap_plot)
}
