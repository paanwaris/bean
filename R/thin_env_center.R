#' Thin occurrence data to grid cell centers (deterministic)
#'
#' @description This function thins species occurrence records by finding all
#' occupied cells in a 2D environmental grid and returning a single new point
#' at the exact center of each of those cells. This is a deterministic method.
#'
#' @param data A data frame containing species occurrences and environmental data.
#' @param env_vars A character vector of length two specifying the names of the
#'   environmental variables to use.
#' @param grid_resolution A numeric vector of length one or two specifying the
#'   resolution(s) for the grid axes. If length one, it is used for both axes.
#' @param verbose (logical) If TRUE, prints progress messages. Default = TRUE.
#'
#' @return An object of class \code{bean_thinned_center}.
#'
#' @export
#' @examples
#' \dontrun{
#' # 1. Load and prepare the data
#' occ_file <- system.file("extdata", "P_maniculatus_samples.csv", package = "bean")
#' occ_data <- read.csv(occ_file)
#'
#' # 2. Thin the data to grid cell centers
#' thinned_center_obj <- thin_env_center(
#'   data = occ_data,
#'   env_vars = c("BIO1", "BIO12"),
#'   grid_resolution = c(0.2, 0.2)
#' )
#'
#' # 3. Print the summary
#' print(thinned_center_obj)
#'
#' # 4. Access the new centroid points
#' thinned_centers_df <- thinned_center_obj$thinned_points
#' }
thin_env_center <- function(data, env_vars, grid_resolution, verbose = TRUE) {
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
    if (verbose) message("No complete observations to process.")
    results <- list(thinned_points = clean_data[, env_vars], original_points = clean_data[, env_vars])
    class(results) <- "bean_thinned_center"
    return(results)
  }

  # --- Calculate Centroids ---
  centroids <- clean_data %>%
    dplyr::mutate(
      center_x = floor(!!env_var1_sym / grid_resolution[1]) * grid_resolution[1] + (grid_resolution[1] / 2),
      center_y = floor(!!env_var2_sym / grid_resolution[2]) * grid_resolution[2] + (grid_resolution[2] / 2)
    )

  # --- Get Unique Centroids ---
  thinned_df <- centroids %>%
    dplyr::distinct(center_x, center_y)

  colnames(thinned_df) <- env_vars

  # --- Construct S3 Object ---
  results <- list(
    thinned_points = thinned_df,
    original_points = clean_data[, env_vars]
  )
  class(results) <- "bean_thinned_center"

  return(results)
}

#' @export
print.bean_thinned_center <- function(x, ...) {
  cat("--- Bean Deterministic Thinning Results ---\n\n")
  cat(sprintf("Thinned %d original points to %d unique grid cell centers.\n",
              nrow(x$original_points), nrow(x$thinned_points)))
}

