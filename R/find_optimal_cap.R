#' Find optimal density caps based on a target thinning percentage
#'
#' @description This function searches for optimal density caps using two criteria:
#' 1) the cap that results in a point count closest to the target percentage, and
#' 2) the cap that results in the closest point count AT OR ABOVE the target.
#'
#' @param data A data frame containing species occurrences and environmental data.
#' @param env_vars A character vector of length two specifying the names of the
#'   columns to be used as the axes of the environmental space.
#' @param grid_resolution A numeric vector of length one or two specifying the
#'   resolution(s) for the grid axes. If length one, it is used for both axes.
#' @param target_percent A numeric value (0-1) for the target proportion of
#'   points to retain.
#' @param verbose (logical) If TRUE, prints progress messages. Default = TRUE.
#'
#' @return An object of class \code{bean_optimization}.
#'
#' @export
#' @importFrom dplyr mutate count pull tibble add_row slice_min filter
#' @importFrom rlang sym
#' @importFrom utils txtProgressBar setTxtProgressBar
find_optimal_cap <- function(data, env_vars, grid_resolution, target_percent, verbose = TRUE) {
  # --- Input Validation and Robust NA/Inf Handling ---
  if (!all(env_vars %in% names(data))) {
    stop("One or both specified env_vars not found in the data frame.")
  }

  # Ensure grid_resolution is a vector of length 2
  if (length(grid_resolution) == 1) {
    grid_resolution <- c(grid_resolution, grid_resolution)
  } else if (length(grid_resolution) != 2) {
    stop("grid_resolution must be a numeric vector of length 1 or 2.")
  }

  env_var1_sym <- rlang::sym(env_vars[1])
  env_var2_sym <- rlang::sym(env_vars[2])

  clean_data <- data %>%
    dplyr::filter(is.finite(!!env_var1_sym) & is.finite(!!env_var2_sym))

  if (verbose && nrow(clean_data) < nrow(data)) {
    warning(paste(nrow(data) - nrow(clean_data),
                  "rows with non-finite values were removed."),
            call. = FALSE)
  }

  if (nrow(clean_data) == 0) {
    stop("No complete observations to run optimization.")
  }

  target_point_count <- floor(nrow(clean_data) * target_percent)

  max_density <- clean_data %>%
    dplyr::mutate(
      env_cell_id = paste(
        floor(!!env_var1_sym / grid_resolution[1]), # Use first resolution
        floor(!!env_var2_sym / grid_resolution[2]), # Use second resolution
        sep = "_"
      )
    ) %>%
    dplyr::count(env_cell_id) %>%
    dplyr::pull(n) %>%
    max(na.rm = TRUE)

  if (!is.finite(max_density) || max_density < 1) {
    if (verbose) message("Could not determine a valid maximum density. No thinning will be performed.")
    max_density <- 1
  }

  cap_candidates <- 1:max_density

  search_results <- dplyr::tibble(
    cap = integer(),
    thinned_count = integer()
  )

  if (verbose) {
    cat("Searching for optimal cap...\n")
    pb <- utils::txtProgressBar(min = 0, max = max_density, style = 3)
  }

  for (cap in cap_candidates) {
    thinned_data <- thin_env_density(
      clean_data, env_vars, grid_resolution, cap, verbose = FALSE
    )$thinned_data

    search_results <- search_results %>%
      dplyr::add_row(cap = cap, thinned_count = nrow(thinned_data))

    if (verbose) utils::setTxtProgressBar(pb, cap)
  }

  if (verbose) close(pb)

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
print.bean_optimization <- function(x, ...) {
  cat("--- Bean Optimization Results ---\n\n")
  cat("Recommendation for 'Closest to Target':\n")
  cat(sprintf("  - Best Cap: %d\n", x$best_cap_closest))
  cat(sprintf("  - Retained Points: %d\n\n", x$retained_points_closest))

  cat("Recommendation for 'Closest Above Target':\n")
  if (is.na(x$best_cap_above_target)) {
    cat("  - No cap found that was at or above the target percentage.\n")
  } else {
    cat(sprintf("  - Best Cap: %d\n", x$best_cap_above_target))
    cat(sprintf("  - Retained Points: %d\n\n", x$retained_points_above_target))
  }
}
#' Plot bean_optimization results
#'
#' Creates a diagnostic plot from the output of \code{\link{find_optimal_cap}}.
#'
#' @param x An object of class \code{bean_optimization}.
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_vline geom_hline labs theme_bw
plot.bean_optimization <- function(x, ...) {
  # --- Create Plot ---
  cap_plot <- ggplot2::ggplot(x$search_results, ggplot2::aes(x = cap, y = thinned_count)) +
    ggplot2::geom_line(color = "gray50") +
    ggplot2::geom_point(color = "black") +
    ggplot2::geom_hline(yintercept = x$parameters$target_point_count, linetype = "dashed", color = "red") +
    ggplot2::geom_vline(xintercept = x$best_cap_closest, linetype = "dashed", color = "blue") +
    ggplot2::labs(
      title = "Search for Optimal Density Cap",
      x = "Maximum Points per Cell (Cap)",
      y = "Number of Points Retained",
      caption = paste0(
        "Red line: Target count (", x$parameters$target_point_count, ")\n",
        "Blue line: 'Closest' cap (", x$best_cap_closest, ")"
      )
    ) +
    ggplot2::theme_bw()

  return(cap_plot)
}
