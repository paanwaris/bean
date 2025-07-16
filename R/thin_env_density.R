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
#'
#' @return An object of class \code{bean_thinned_density}.
#'
#' @export
#' @importFrom dplyr mutate group_by group_modify ungroup slice_sample select filter
#' @importFrom rlang sym
#' @examples
#' \dontrun{
#'   # Assume 'my_occ_data' is a data frame with temp_mean and salinity_mean columns
#'   set.seed(123) # Set seed before calling for reproducibility
#'   thinned_data_obj <- thin_env_density(
#'     data = my_occ_data,
#'     env_vars = c("temp_mean", "salinity_mean"),
#'     grid_resolution = 0.1,
#'     max_per_cell = 1
#'   )
#' }
thin_env_density <- function(data, env_vars, grid_resolution, max_per_cell) {
  # --- Input Validation and NA Handling ---
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
    message("No complete observations to thin.")
    # Return an object with the correct structure, but empty
    results <- list(
      thinned_data = clean_data,
      n_original = 0,
      n_thinned = 0
    )
    class(results) <- "bean_thinned_density"
    return(results)
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

  # --- FIX: Construct and return the S3 object ---
  results <- list(
    thinned_data = thinned_df,
    n_original = nrow(clean_data),
    n_thinned = nrow(thinned_df)
  )
  class(results) <- "bean_thinned_density"

  return(results)
}

#' @export
print.bean_thinned_density <- function(x, ...) {
  cat("--- Bean Stochastic Thinning Results ---\n\n")
  cat(sprintf("Thinned %d original points to %d points.\n", x$n_original, x$n_thinned))
  if (x$n_original > 0) {
    cat(sprintf("This represents a retention of %.1f%% of the data.\n", 100 * (x$n_thinned / x$n_original)))
  }
}
