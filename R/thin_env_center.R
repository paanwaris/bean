#' Deterministic centroid
#'
#' @description This function thins species occurrence records by finding all
#' occupied cells in a 2D environmental grid and returning a single new point
#' at the exact center of each of those cells. This is a deterministic method.
#'
#' @param data A data.frame containing species occurrence coordinates and the environmental variables.
#' @param env_vars A character vector specifying the column names in data that represent the environmental variables to be used in the analysis.
#' @param grid_resolution A numeric vector of length one or two specifying the
#'   resolution(s) for the grid axes. If length one, it is used for both axes. See the Details section of \code{\link{find_env_resolution}} for a full explanation
#' @return An object of class \code{bean_thinned_center}. which is a list containing:
#'   \item{thinned_points}{A data.frame with two columns representing the new
#'     points at the center of each occupied environmental grid cell.}
#'   \item{n_original}{An integer representing the number of complete occurrence
#'     records in the input data.}
#'   \item{n_thinned}{An integer representing the number of unique grid cells
#'     that were occupied, which is also the number of points returned.}
#'   \item{parameters}{A list of the key parameters used, such as whether
#'     scaling was applied.}
#' @seealso \code{\link{find_env_resolution}}
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
    message("No complete observations to process.")
    empty_df <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(empty_df) <- env_vars
    results <- list(
      thinned_points = empty_df,
      n_original = 0,
      n_thinned = 0,
      parameters = list()
    )
    class(results) <- "bean_thinned_center"
    return(results)
  }

  # Data is assumed to be pre-scaled
  data_to_process <- clean_data

  # --- Calculate Centroids ---
  centroids_df <- data_to_process %>%
    dplyr::mutate(
      center_x = floor(!!env_var1_sym / grid_resolution[1]) * grid_resolution[1] + (grid_resolution[1] / 2),
      center_y = floor(!!env_var2_sym / grid_resolution[2]) * grid_resolution[2] + (grid_resolution[2] / 2)
    ) %>%
    dplyr::distinct(.data$center_x, .data$center_y)

  # Set final column names
  colnames(centroids_df) <- env_vars

  # --- Construct S3 Object ---
  results <- list(
    thinned_points = centroids_df,
    n_original = nrow(clean_data),
    n_thinned = nrow(centroids_df),
    parameters = list()
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
