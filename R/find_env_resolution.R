#' Find an objective grid resolution using pairwise environmental distances
#'
#' @description This function assumes the environmental variables in the input
#' data have already been scaled (e.g., using `scale()`). It calculates the
#' distribution of all pairwise Euclidean distances between points in this
#' environmental space. The distance at a specified lower quantile of this
#' distribution is returned as a suggested grid resolution.
#'
#' @param data A data frame containing species occurrences and pre-scaled
#'   environmental data.
#' @param env_vars A character vector of length two specifying the names of the
#'   environmental variables to use.
#' @param quantile (numeric) The quantile of pairwise distances to use for the
#'   resolution. A smaller value (e.g., 0.05 or 0.1) is recommended.
#'   Default = 0.1.
#' @param verbose (logical) If TRUE, prints progress messages and warnings.
#'   Default = TRUE.
#'
#' @return An object of class \code{bean_resolution}.
#'
#' @export
#' @importFrom stats dist quantile
find_env_resolution <- function(data, env_vars, quantile = 0.1, verbose = TRUE) {
  # --- Input Validation and Data Cleaning ---
  if (!all(env_vars %in% names(data))) {
    stop("One or both specified env_vars not found in the data frame.")
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

  if (nrow(clean_data) < 3) {
    stop("At least 3 complete observations are needed to calculate resolution.")
  }

  # --- Calculate Pairwise Distances ---
  if (verbose) cat("Calculating pairwise environmental distances...\n")
  env_data <- clean_data[, env_vars]
  pairwise_dists <- stats::dist(env_data)

  # --- Find Suggested Resolution ---
  suggested_res <- stats::quantile(pairwise_dists, probs = quantile, na.rm = TRUE)

  if (verbose) cat(sprintf("Resolution at %.2f quantile: %f\n", quantile, suggested_res))

  # --- Construct S3 Object ---
  results <- list(
    suggested_resolution = as.numeric(suggested_res),
    distance_distribution = as.numeric(pairwise_dists)
  )

  class(results) <- "bean_resolution"
  return(results)
}

# Custom print and plot methods for the new class
#' @export
print.bean_resolution <- function(x, ...) {
  cat("--- Bean Environmental Resolution Analysis (Pairwise Distance) ---\n\n")
  cat(sprintf("Suggested Grid Resolution: %f\n", x$suggested_resolution))
  cat("(This is the distance at the specified quantile of all pairwise distances)\n\n")
  cat("To see the full distance distribution, run plot(your_results_object).\n")
}

#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline labs theme_bw
plot.bean_resolution <- function(x, ...) {
  plot_data <- data.frame(distances = x$distance_distribution)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = distances)) +
    ggplot2::geom_histogram(bins = 50, fill = "grey80", color = "black") +
    ggplot2::geom_vline(xintercept = x$suggested_resolution, linetype = "dashed", color = "blue", linewidth = 1) +
    ggplot2::labs(
      title = "Distribution of Pairwise Environmental Distances",
      subtitle = "Blue line indicates the suggested grid resolution at the specified quantile",
      x = "Environmental Distance (scaled)",
      y = "Frequency"
    ) +
    ggplot2::theme_bw()
}
