#' Thin occurrence data in n-dimensional environmental space
#'
#' @description This function thins species occurrence records in an n-dimensional
#' environmental space by randomly sampling exactly one point from each occupied
#' n-dimensional grid cell (hypercube).
#'
#' @param data A data.frame containing species occurrences and pre-scaled
#'   environmental variables, typically the output of `prepare_bean()`.
#' @param env_vars A character vector of two or more column names representing
#'   the environmental variables (dimensions) to use for thinning.
#' @param grid_resolution A numeric vector of resolutions for each environmental
#'   axis. Its length must match the length of `env_vars`.
#' @param seed (numeric) An optional random seed for reproducibility. If provided,
#'   the random number generator state is safely isolated to this function call
#'   and will not affect the global environment. Default = NULL.
#'
#' @return An object of class `bean_thinned`, which is a list containing:
#'   \item{thinned_data}{A data.frame containing the occurrence records that were retained after the thinning process.}
#'   \item{n_original}{An integer representing the number of complete occurrence records in the input data before thinning.}
#'   \item{n_thinned}{An integer representing the number of occurrence records remaining after thinning.}
#'   \item{parameters}{A list of the key parameters used during the thinning process.}
#' @export
#' @examples
#' \dontrun{
#' # Assume 'prepared_pca' is the output from prepare_bean(..., transform = "pca")
#' # and contains columns PC1, PC2, PC3.
#'
#' # Thin the data in 3D environmental space, retaining 1 point per grid cell
#' thinned_3d <- thin_env_nd(
#'   data = prepared_pca$prepared_data,
#'   env_vars = c("PC1", "PC2", "PC3"),
#'   grid_resolution = c(0.5, 0.5, 0.5), # One resolution value for each axis
#'   seed = 123
#' )
#'
#' # Print the summary
#' print(thinned_3d)
#' }
thin_env_nd <- function(data, env_vars, grid_resolution, seed = NULL) {

  # --- Safe Seed Handling ---
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
    } else {
      on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
    }
    set.seed(seed)
  }

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
      parameters = list(grid_resolution = grid_resolution, seed = seed)
    )
    class(results) <- "bean_thinned"
    return(results)
  }

  # --- Dynamic N-Dimensional Grid Cell ID Creation ---
  env_data <- clean_data[, env_vars, drop = FALSE]
  gridded_vals <- t(t(env_data) / grid_resolution)
  cell_ids <- apply(floor(gridded_vals), 1, paste, collapse = "_")

  # Split the row indices by their grid cell ID
  split_indices <- split(seq_len(nrow(clean_data)), cell_ids)

  # Randomly sample exactly 1 row index from each cell
  sampled_indices <- sapply(split_indices, function(idx) {
    if (length(idx) == 1) {
      return(idx)
    } else {
      # sample.int is safer and faster in base R
      return(idx[sample.int(length(idx), size = 1)])
    }
  })

  # Subset the original data using those sampled indices
  thinned_df <- clean_data[sampled_indices, , drop = FALSE]

  # --- Construct S3 Object ---
  results <- list(
    thinned_data = as.data.frame(thinned_df),
    n_original = nrow(clean_data),
    n_thinned = nrow(thinned_df),
    parameters = list(grid_resolution = grid_resolution, seed = seed)
  )
  class(results) <- "bean_thinned"
  return(results)
}

#' Print a summary of bean_thinned results
#'
#' @param x An object of class `bean_thinned`.
#' @param ... Additional arguments (not used).
#' @return Invisibly returns the input object `x`.
#' @export
#' @keywords internal
print.bean_thinned <- function(x, ...) {
  cat("--- Bean Stochastic Thinning Results ---\n\n")
  cat(sprintf("Thinned %d original points to %d points.\n", x$n_original, x$n_thinned))
  if (x$n_original > 0) {
    cat(sprintf("This represents a retention of %.1f%% of the data.\n", 100 * (x$n_thinned / x$n_original)))
  }
  cat("\n--------------------------------------\n")
  invisible(x)
}
