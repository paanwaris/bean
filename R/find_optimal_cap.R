#' Find the optimal density cap to achieve a target thinning percentage
#'
#' This function searches for the density cap that results in a thinned dataset
#' closest in size to a user-specified target percentage of the original data.
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
#' @return A list containing the best cap found, the resulting point count,
#'   and a data frame of the full search results.
#'
#' @export
#' @importFrom dplyr mutate count pull tibble add_row slice_min
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
    max()

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

  # Find the best result
  best_result <- search_results %>%
    dplyr::mutate(difference = abs(thinned_count - target_point_count)) %>%
    dplyr::slice_min(order_by = difference, n = 1, with_ties = TRUE) %>%
    dplyr::slice_min(order_by = cap, n = 1)

  return(list(
    best_cap = best_result$cap,
    retained_points = best_result$thinned_count,
    search_results = search_results
  ))
}
