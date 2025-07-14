#' Thin occurrence data based on environmental density
#'
#' This function thins species occurrence records in a 2D environmental space
#' defined by two user-specified variables. It applies a grid to this space
#' and samples points from cells that exceed a given density cap.
#'
#' @param data A data frame containing species occurrences and environmental data.
#' @param env_vars A character vector of length two specifying the names of the
#'   columns to be used as the axes of the environmental space (e.g.,
#'   c("temp_mean", "salinity_mean")).
#' @param grid_resolution A single numeric value specifying the resolution of the
#'   grid to be applied to the environmental space.
#' @param max_per_cell An integer specifying the maximum number of points to
#'   retain per grid cell.
#' @param seed An optional integer to set the random seed for reproducibility
#'   of the sampling step.
#'
#' @return A data frame containing the thinned set of occurrence points.
#'
#' @export
#' @importFrom dplyr mutate group_by group_modify ungroup slice_sample select
#' @examples
#' \dontrun{
#'   # Assume 'my_occ_data' is a data frame with temp_mean and salinity_mean columns
#'   thinned_data <- thin_env_density(
#'     data = my_occ_data,
#'     env_vars = c("temp_mean", "salinity_mean"),
#'     grid_resolution = 0.1,
#'     max_per_cell = 1,
#'     seed = 123
#'   )
#'
#'   # Compare number of points
#'   nrow(my_occ_data)
#'   nrow(thinned_data)
#' }
thin_env_density <- function(data, env_vars, grid_resolution, max_per_cell, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Ensure env_vars are present in the data
  if (!all(env_vars %in% names(data))) {
    stop("One or both specified env_vars not found in the data frame.")
  }

  # Use non-standard evaluation to handle variable column names
  env_var1 <- rlang::sym(env_vars[1])
  env_var2 <- rlang::sym(env_vars[2])

  thinned_df <- data %>%
    # 1. Create a unique ID for each cell in the environmental grid
    dplyr::mutate(
      env_cell_id = paste(
        floor(!!env_var1 / grid_resolution),
        floor(!!env_var2 / grid_resolution),
        sep = "_"
      )
    ) %>%
    # 2. Group data by the grid cell ID
    dplyr::group_by(env_cell_id) %>%
    # 3. For each group (cell), check if it exceeds the cap
    dplyr::group_modify(~ {
      if (nrow(.x) > max_per_cell) {
        # If over capacity, randomly sample 'max_per_cell' points
        .x %>% dplyr::slice_sample(n = max_per_cell)
      } else {
        # If at or under capacity, keep all points
        .x
      }
    }) %>%
    # 4. Ungroup and remove the temporary cell ID column
    dplyr::ungroup() %>%
    dplyr::select(-env_cell_id)

  return(thinned_df)
}
