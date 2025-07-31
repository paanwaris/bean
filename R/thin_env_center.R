#' Deterministic centroid
#'
#' @description This function thins species occurrence records by finding all
#' occupied cells in a 2D environmental grid and returning a single new point
#' at the exact center of each of those cells. This is a deterministic method.
#'
#' @param data A data.frame containing species occurrence coordinates and the environmental variables.
#' @param env_vars A character vector specifying the column names in data that represent the environmental variables to be used in the analysis.
#' @param grid_resolution A numeric vector of length one or two specifying the
#'   resolution(s) for the grid axes. If length one, it is used for both axes. See the Details section of \code{\link{find_optimal_cap}} for a full explanation
#' @return An object of class \code{bean_thinned_center}. which is a list containing:
#'   \item{thinned_points}{A data.frame with two columns representing the new
#'     points at the center of each occupied environmental grid cell.}
#'   \item{n_original}{An integer representing the number of complete occurrence
#'     records in the input data.}
#'   \item{n_thinned}{An integer representing the number of unique grid cells
#'     that were occupied, which is also the number of points returned.}
#'   \item{parameters}{A list of the key parameters used, such as whether
#'     scaling was applied.}
#' @seealso \code{\link{find_optimal_cap}}
#' @export
#' @examples
#' \dontrun{
#' # 1. Create environmental data
#' set.seed(81)
#' env_data <- data.frame(
#' BIO1 = c(0.1, 0.2, 1.1, 1.2, 1.3),
#' BIO12 = c(0.1, 0.2, 2.1, 2.2, 2.3)
#' )
#'
#' # 2. Thin the data to grid cell centers
#' thinned_center_obj <- thin_env_center(
#'   data = env_data,
#'   env_vars = c("BIO1", "BIO12"),
#'   grid_resolution = c(0.1, 0.2)
#' )
#'
#' # 3. Print the summary
#' print(thinned_center_obj)
#' }
thin_env_center <- function(data, env_vars, grid_resolution) {
  # --- Input Validation ---
  n_dims <- length(env_vars)
  if (n_dims < 1) {
    stop("At least one environmental variable must be provided in `env_vars`")
  }
  if (!all(env_vars %in% names(data))) {
    stop("One or more `env_vars` not found in the data frame")
  }

  # Validate and expand grid_resolution
  if (length(grid_resolution) == 1) {
    grid_resolution <- rep(grid_resolution, n_dims)
  } else if (length(grid_resolution) != n_dims) {
    stop(sprintf("`grid_resolution` must have length 1 or %d", n_dims))
  }

  # --- Data Cleaning ---
  clean_data <- data %>%
    # Keep only rows with complete, finite values for all selected env_vars
    dplyr::filter(dplyr::if_all(dplyr::all_of(env_vars), is.finite))

  n_original <- nrow(clean_data)

  if (n_original == 0) {
    message("No complete observations to process.")
    empty_df <- data.frame(matrix(ncol = n_dims, nrow = 0))
    colnames(empty_df) <- env_vars
    results <- list(
      thinned_points = empty_df,
      n_original = 0,
      n_thinned = 0,
      parameters = list(grid_resolution = grid_resolution)
    )
    class(results) <- "bean_thinned_center"
    return(results)
  }

  # --- Calculate Centroids in N-Dimensions ---

  # Create a named vector for easy lookup of resolution within across()
  resolutions_named <- setNames(grid_resolution, env_vars)

  centroids_df <- clean_data %>%
    # For each environmental variable, calculate its cell center
    dplyr::mutate(dplyr::across(
      dplyr::all_of(env_vars),
      ~ floor(.x / resolutions_named[dplyr::cur_column()]) * resolutions_named[dplyr::cur_column()] + (resolutions_named[dplyr::cur_column()] / 2)
    )) %>%
    # Keep only the unique combinations of cell centers
    dplyr::distinct(dplyr::across(dplyr::all_of(env_vars)))

  # --- Construct S3 Object ---
  results <- list(
    thinned_points = as.data.frame(centroids_df),
    n_original = n_original,
    n_thinned = nrow(centroids_df),
    parameters = list(grid_resolution = grid_resolution)
  )
  class(results) <- "bean_thinned_center"
  return(results)
}

#' Print a summary of bean_thinned_center results
#'
#' @param x An object of class \code{bean_thinned_center}.
#' @param ... Additional arguments (not used).
#' @return Invisibly returns the input object \code{x}.
#' @export
#' @keywords internal
print.bean_thinned_center <- function(x, ...) {
  cat("--- Bean Deterministic Thinning Results ---\n\n")
  cat(sprintf("Thinned %d original points to %d unique grid cell centers.\n",
              x$n_original, x$n_thinned))
  cat("\n-----------------------------------------\n")
  invisible(x)
}
