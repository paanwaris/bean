#' Find optimal density caps based on a target thinning percentage
#'
#' This function searches for optimal density caps using two criteria:
#' 1) the cap that results in a point count closest to the target percentage, and
#' 2) the smallest cap that retains at least the target percentage.
#'
#' @param data A data frame containing species occurrences and environmental data.
#' @param env_vars A character vector of length two specifying the names of the
#'   columns to be used as the axes of the environmental space.
#' @param grid_resolution A single numeric value specifying the resolution of the
#'   grid.
#' @param target_percent A numeric value (0-1) for the target proportion of
#'   points to retain.
#' @param seed An optional integer to set the random seed for reproducibility.
#'
#' @return A list containing:
#'   \item{best_cap_closest}{The cap that yields a count closest to the target.}
#'   \item{retained_points_closest}{The point count for the 'closest' cap.}
#'   \item{best_cap_conservative}{The smallest cap that retains at least the target %.}
#'   \item{retained_points_conservative}{The point count for the 'conservative' cap.}
#'   \item{search_results}{A data frame of the full search results.}
#'   \item{plot}{A ggplot object visualizing the search.}
#'
#' @export
#' @importFrom dplyr mutate count pull tibble add_row slice_min filter
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_vline geom_hline labs theme_bw
find_optimal_cap <- function(data, env_vars, grid_resolution, target_percent, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Use non-standard evaluation
  env_var1 <- rlang::sym(env_vars[1])
  env_var2 <- rlang::sym(env_vars[2])

  # Calculate target number of points
  target_point_count <- floor(nrow(data) * target_percent)

  # Find the maximum number of points in any single grid cell
  max_density <- data %>%
    dplyr::mutate(
      env_cell_id = paste(
        floor(!!env_var1 / grid_resolution),
        floor(!!env_var2 / grid_resolution),
        sep = "_"
      )
    ) %>%
    dplyr::count(env_cell_id) %>%
    dplyr::pull(n) %>%
    max(na.rm = TRUE)

  cap_candidates <- 1:max_density

  # Data frame to store search results
  search_results <- dplyr::tibble(
    cap = integer(),
    thinned_count = integer()
  )

  # Loop through each candidate cap
  for (cap in cap_candidates) {
    thinned_data <- thin_env_density(
      data, env_vars, grid_resolution, cap, seed
    )
    search_results <- search_results %>%
      dplyr::add_row(cap = cap, thinned_count = nrow(thinned_data))
  }

  # --- Criterion 1: Find the best result based on 'closest to target' ---
  best_result_closest <- search_results %>%
    dplyr::mutate(difference = abs(thinned_count - target_point_count)) %>%
    dplyr::slice_min(order_by = difference, n = 1, with_ties = TRUE) %>%
    dplyr::slice_min(order_by = cap, n = 1) # Tie-break by choosing smaller cap

  # --- Criterion 2: Find the 'conservative' result (keep at least target %) ---
  best_result_conservative <- search_results %>%
    dplyr::filter(thinned_count >= target_point_count) %>%
    dplyr::slice_min(order_by = cap, n = 1, with_ties = FALSE)

  # Handle case where no cap meets the conservative criteria
  if (nrow(best_result_conservative) == 0) {
    best_cap_conservative_val <- NA
    retained_points_conservative <- NA
  } else {
    best_cap_conservative_val <- best_result_conservative$cap
    retained_points_conservative <- best_result_conservative$thinned_count
  }

  # Create a plot to visualize the search
  cap_plot <- ggplot2::ggplot(search_results, ggplot2::aes(x = cap, y = thinned_count)) +
    ggplot2::geom_line(color = "gray50") +
    ggplot2::geom_point(color = "black") +
    ggplot2::geom_hline(yintercept = target_point_count, linetype = "dashed", color = "red") +
    ggplot2::geom_vline(xintercept = best_result_closest$cap, linetype = "dashed", color = "blue") +
    ggplot2::labs(
      title = "Search for Optimal Density Cap",
      x = "Maximum Points per Cell (Cap)",
      y = "Number of Points Retained",
      caption = paste0(
        "Red line: Target count (", target_point_count, ")\n",
        "Blue line: 'Closest' cap (", best_result_closest$cap, ")"
      )
    ) +
    ggplot2::theme_bw()

  return(list(
    best_cap_closest = best_result_closest$cap,
    retained_points_closest = best_result_closest$thinned_count,
    best_cap_conservative = best_cap_conservative_val,
    retained_points_conservative = retained_points_conservative,
    search_results = search_results,
    plot = cap_plot
  ))
}
