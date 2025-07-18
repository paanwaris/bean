#' Thin occurrence data based on environmental density (stochastic)
#'
#' @description This function thins species occurrence records in a 2D environmental
#' space by randomly sampling a specified number of points from each occupied
#' grid cell.
#'
#' @param data A data frame containing species occurrences and environmental data.
#' @param env_vars A character vector of length two specifying the names of the
#'   environmental variables to use.
#' @param grid_resolution A numeric vector of length one or two specifying the
#'   resolution(s) for the grid axes. If length one, it is used for both axes.
#' @param max_per_cell An integer specifying the maximum number of points to
#'   retain per grid cell.
#' @param verbose (logical) If TRUE, prints progress messages. Default = TRUE.
#'
#' @return An object of class \code{bean_thinned_density}.
#'
#' @export
thin_env_density <- function(data, env_vars, grid_resolution, max_per_cell, verbose = TRUE) {
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
    if (verbose) message("No complete observations to thin.")
    results <- list(thinned_data = clean_data, n_original = 0, n_thinned = 0)
    class(results) <- "bean_thinned_density"
    return(results)
  }

  # --- Thinning Logic ---
  thinned_df <- clean_data %>%
    dplyr::mutate(
      env_cell_id = paste(
        floor(!!env_var1_sym / grid_resolution[1]), # Use first resolution
        floor(!!env_var2_sym / grid_resolution[2]), # Use second resolution
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

  # --- Construct S3 Object ---
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
