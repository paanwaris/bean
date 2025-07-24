#' Stochastic centroid
#'
#' @description This function thins species occurrence records in a 2D environmental
#' space by randomly sampling a specified number of points from each occupied
#' grid cell. This is a stochastic method.
#'
#' @param data A data.frame containing species occurrence coordinates and the environmental variables.
#' @param env_vars A character vector specifying the column names in data that represent the environmental variables to be used in the analysis.
#' @param grid_resolution A numeric vector of length one or two specifying the
#'   resolution(s) for the grid axes. If length one, it is used for both axes. See the Details section of \code{\link{find_env_resolution}} for a full explanation
#' @param max_per_cell An integer specifying the maximum number of points to
#'   retain per grid cell. See the Details section of \code{\link{find_optimal_cap}} for a full explanation
#' @return An object of class \code{bean_thinned_density}, which is a list containing:
#'   \item{thinned_data}{A data.frame containing the occurrence records that were retained after the thinning process.}
#'   \item{n_original}{An integer representing the number of complete occurrence records in the input data before thinning.}
#'   \item{n_thinned}{An integer representing the number of occurrence records remaining after thinning.}
#' @seealso \code{\link{find_optimal_cap}}, \code{\link{find_env_resolution}}
#' @export
#' @examples
#' \dontrun{
#' # 1. Create environmental data
#' env_data <- data.frame(
#' BIO1 = c(rep(0.1, 5), rep(1.1, 3)),
#' BIO12 = c(rep(0.2, 5), rep(2.2, 3)),
#' species = "A"
#' )
#'
#' # 2. Thin the data to a max of 1 point per cell
#' set.seed(81) # For reproducible results
#' thinned_obj <- thin_env_density(
#'   data = env_data,
#'   env_vars = c("BIO1", "BIO12"),
#'   grid_resolution = c(0.2, 0.2),
#'   max_per_cell = 1
#' )
#'
#' # 3. Print the summary
#' print(thinned_obj)
#' }
thin_env_density <- function(data, env_vars, grid_resolution, max_per_cell) {
  # --- Input Validation ---
  if (!all(env_vars %in% names(data))) {
    stop("One or both specified env_vars not found in the data frame.")
  }
  if (length(grid_resolution) == 1) {
    grid_resolution <- c(grid_resolution, grid_resolution)
  } else if (length(grid_resolution) != 2) {
    stop("grid_resolution must be a numeric vector of length 1 or 2.")
  }

  env_var1_sym <- rlang::sym(env_vars[1])
  env_var2_sym <- rlang::sym(env_vars[2])

  # --- Data Cleaning ---
  clean_data <- data %>%
    dplyr::filter(is.finite(!!env_var1_sym) & is.finite(!!env_var2_sym))

  if (nrow(clean_data) == 0) {
    message("No complete observations to thin.")
    results <- list(
      thinned_data = clean_data,
      n_original = 0,
      n_thinned = 0
    )
    class(results) <- "bean_thinned_density"
    return(results)
  }

  # Data is assumed to be pre-scaled
  # --- Thinning Logic ---
  thinned_df <- clean_data %>%
    dplyr::mutate(
      env_cell_id = paste(
        floor(!!env_var1_sym / grid_resolution[1]),
        floor(!!env_var2_sym / grid_resolution[2]),
        sep = "_"
      )
    ) %>%
    dplyr::group_by(env_cell_id) %>%
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
    n_thinned = nrow(thinned_df)
  )
  class(results) <- "bean_thinned_density"
  return(results)
}

#' Print a summary of bean_thinned_density results
#'
#' @param x An object of class \code{bean_thinned_density}.
#' @param ... Additional arguments (not used).
#' @return Invisibly returns the input object \code{x}.
#' @export
#' @keywords internal
print.bean_thinned_density <- function(x, ...) {
  cat("--- Bean Stochastic Thinning Results ---\n\n")
  cat(sprintf("Thinned %d original points to %d points.\n", x$n_original, x$n_thinned))
  if (x$n_original > 0) {
    cat(sprintf("This represents a retention of %.1f%% of the data.\n", 100 * (x$n_thinned / x$n_original)))
  }
  cat("\n--------------------------------------\n")
  invisible(x)
}
