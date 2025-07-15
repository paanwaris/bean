#' Find optimal density caps based on a target thinning percentage
#'
#' This function searches for optimal density caps using two criteria:
#' 1) the cap that results in a point count closest to the target percentage, and
#' 2) the cap that results in the closest point count AT OR ABOVE the target.
#'
#' @param data A data frame containing species occurrences and environmental data.
#' @param env_vars A character vector of length two specifying the names of the
#'   columns to be used as the axes of the environmental space.
#' @param grid_resolution A single numeric value specifying the resolution of the
#'   grid.
#' @param target_percent A numeric value (0-1) for the target proportion of
#'   points to retain.
#'
#' @return An object of class \code{bean_optimization} containing the optimal
#'   cap recommendations and the full search results, which can be plotted.
#'
#' @export
#' @importFrom dplyr mutate count pull tibble add_row slice_min filter
#' @importFrom rlang sym
find_optimal_cap <- function(data, env_vars, grid_resolution, target_percent) {
  # --- Input Validation and Robust NA/Inf Handling ---
  if (!all(env_vars %in% names(data))) {
    stop("One or both specified env_vars not found in the data frame.")
  }

  env_var1_sym <- rlang::sym(env_vars[1])
  env_var2_sym <- rlang::sym(env_vars[2])

  clean_data <- data %>%
    dplyr::filter(is.finite(!!env_var1_sym) & is.finite(!!env_var2_sym))

  if (nrow(clean_data) < nrow(data)) {
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
        floor(!!env_var1_sym / grid_resolution),
        floor(!!env_var2_sym / grid_resolution),
        sep = "_"
      )
    ) %>%
    dplyr::count(env_cell_id) %>%
    dplyr::pull(n) %>%
    max(na.rm = TRUE)

  if (!is.finite(max_density) || max_density < 1) {
    message("Could not determine a valid maximum density. No thinning will be performed.")
    max_density <- 1
  }

  cap_candidates <- 1:max_density

  search_results <- dplyr::tibble(
    cap = integer(),
    thinned_count = integer()
  )

  # Note: A set.seed() call should be made *before* this function for reproducibility
  for (cap in cap_candidates) {
    thinned_data <- thin_env_density(
      clean_data, env_vars, grid_resolution, cap
    )
    search_results <- search_results %>%
      dplyr::add_row(cap = cap, thinned_count = nrow(thinned_data))
  }

  # --- Find Optimal Caps ---
  best_result_closest <- search_results %>%
    dplyr::mutate(difference = abs(thinned_count - target_point_count)) %>%
    dplyr::slice_min(order_by = difference, n = 1, with_ties = TRUE) %>%
    dplyr::slice_min(order_by = cap, n = 1)

  best_result_above_target <- search_results %>%
    dplyr::filter(thinned_count >= target_point_count) %>%
    dplyr::slice_min(order_by = thinned_count, n = 1, with_ties = TRUE) %>%
    dplyr::slice_min(order_by = cap, n = 1, with_ties = FALSE)

  # --- Construct the S3 Object ---
  results <- list(
    best_cap_closest = best_result_closest$cap,
    retained_points_closest = best_result_closest$thinned_count,
    best_cap_above_target = best_result_above_target$cap,
    retained_points_above_target = best_result_above_target$thinned_count,
    search_results = search_results,
    parameters = list(
      target_point_count = target_point_count
    )
  )

  # Assign the custom class
  class(results) <- "bean_optimization"

  return(results)
}
