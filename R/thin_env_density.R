#' Thin occurrence data based on environmental density
#'
#' This function thins species occurrence records in a 2D environmental space
#' defined by two user-specified variables. It applies a grid to this space
#' and samples points from cells that exceed a given density cap.
#'
#' @param data A data frame containing species occurrences and environmental data.
#' @param env_vars A character vector of length two specifying the names of the
#'   columns to be used as the axes of the environmental space.
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
#' @importFrom dplyr mutate group_by group_modify ungroup slice_sample select filter all_of
#' @importFrom rlang sym
#' @importFrom stats complete.cases
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
#' }
thin_env_density <- function(data, env_vars, grid_resolution, max_per_cell, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # --- Input Validation and NA Handling ---
  if (!all(env_vars %in% names(data))) {
    stop("One or both specified env_vars not found in the data frame.")
  }

  # Filter out rows with NA in the specified environmental variables
  clean_data <- data[stats::complete.cases(data[, env_vars]), ]

  if (nrow(clean_data) < nrow(data)) {
    warning(paste(nrow(data) - nrow(clean_data),
                  "rows with NA in environmental variables were removed."),
            call. = FALSE)
  }

  if (nrow(clean_data) == 0) {
    message("No complete observations to thin.")
    return(clean_data)
  }

  # Use non-standard evaluation to handle variable column names
  env_var1 <- rlang::sym(env_vars[1])
  env_var2 <- rlang::sym(env_vars[2])

  thinned_df <- clean_data %>%
    dplyr::mutate(
      env_cell_id = paste(
        floor(!!env_var1 / grid_resolution),
        floor(!!env_var2 / grid_resolution),
        sep = "_"
      )
    ) %>%
    dplyr::group_by(env_cell_id) %>%
    dplyr::group_modify(~ {
      if (nrow(.x) > max_per_cell) {
        .x %>% dplyr::slice_sample(n = max_per_cell)
      } else {
        .x
      }
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(-env_cell_id)


  return(thinned_df)
}
