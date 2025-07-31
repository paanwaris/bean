#' Thin occurrence data in n-dimensional environmental space
#'
#' @description This function thins species occurrence records in an n-dimensional
#' environmental space by randomly sampling a specified number of points from
#' each occupied n-dimensional grid cell (hypercube).
#'
#' @param data A data.frame containing species occurrences and pre-scaled
#'   environmental variables, typically the output of `prepare_bean()`.
#' @param env_vars A character vector of two or more column names representing
#'   the environmental variables (dimensions) to use for thinning.
#' @param grid_resolution A numeric vector of resolutions for each environmental
#'   axis. Its length must match the length of `env_vars`.
#' @param max_per_cell An integer specifying the maximum number of points to
#'   retain per grid cell.
#'
#' @return An object of class `bean_thinned_density`, which is a list containing:
#'   \item{thinned_data}{A data.frame containing the occurrence records that were retained after the thinning process.}
#'   \item{n_original}{An integer representing the number of complete occurrence records in the input data before thinning.}
#'   \item{n_thinned}{An integer representing the number of occurrence records remaining after thinning.}
#'   \item{parameters}{A list of the key parameters used during the thinning process.}
#' @export
#' @importFrom dplyr group_by mutate slice_sample ungroup select
#' @examples
#' \dontrun{
#' # Assume 'prepared_pca' is the output from prepare_bean(..., transform = "pca")
#' # and contains columns PC1, PC2, PC3.
#'
#' # Thin the data in 3D environmental space
#' thinned_3d <- thin_env_nd(
#'   data = prepared_pca$prepared_data,
#'   env_vars = c("PC1", "PC2", "PC3"),
#'   grid_resolution = c(0.5, 0.5, 0.5), # One resolution value for each axis
#'   max_per_cell = 1
#' )
#'
#' # Print the summary
#' print(thinned_3d)
#' }
thin_env_nd <- function(data, env_vars, grid_resolution, max_per_cell) {
  # --- Input Validation ---
  if (length(env_vars) < 2) {
    stop("`env_vars` must contain at least two variable names.")
  }
  if (length(env_vars) != length(grid_resolution)) {
    stop("Length of `grid_resolution` must match the length of `env_vars`.")
  }
  if (!all(env_vars %in% names(data))) {
    stop("One or more `env_vars` not found in the data frame.")
  }

  # --- Data Cleaning ---
  clean_data <- data[stats::complete.cases(data[, env_vars]), ]

  if (nrow(clean_data) == 0) {
    message("No complete observations to thin.")
    results <- list(
      thinned_data = clean_data, n_original = 0, n_thinned = 0,
      parameters = list(grid_resolution = grid_resolution)
    )
    class(results) <- "bean_thinned_density"
    return(results)
  }

  # --- Dynamic N-Dimensional Grid Cell ID Creation ---
  env_data <- clean_data[, env_vars, drop = FALSE]
  gridded_vals <- t(t(env_data) / grid_resolution)
  cell_ids <- apply(floor(gridded_vals), 1, paste, collapse = "_")

  # --- Thinning Logic (Corrected) ---
  thinned_df <- clean_data %>%
    dplyr::mutate(env_cell_id = cell_ids) %>%
    dplyr::group_by(env_cell_id) %>%
    # This group_modify block is a more robust way to sample
    dplyr::group_modify(~ {
      if (nrow(.x) > max_per_cell) {
        .x %>% dplyr::slice_sample(n = max_per_cell, replace = FALSE)
      } else {
        .x # Return the group as-is if it's smaller than the cap
      }
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(-env_cell_id)

  # --- Construct S3 Object ---
  results <- list(
    thinned_data = as.data.frame(thinned_df),
    n_original = nrow(clean_data),
    n_thinned = nrow(thinned_df),
    parameters = list(grid_resolution = grid_resolution)
  )
  class(results) <- "bean_thinned_density"
  return(results)
}
