#' Find objective grid resolutions for each environmental axis
#'
#' @description This function assumes the environmental variables in the input
#' data have already been scaled (e.g., using `scale()`). It calculates the
#' distribution of all pairwise 1D distances for each variable. The distance
#' at a specified lower quantile is returned as a suggested grid resolution
#' for each axis.
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
#' @importFrom tidyr pivot_longer
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

  # --- Calculate Resolutions for Each Axis ---
  if (verbose) cat("Calculating pairwise distances for each environmental axis...\n")
  env_data <- clean_data[, env_vars]

  # Calculate resolution for the first variable
  dist1 <- stats::dist(env_data[, 1])
  res1 <- stats::quantile(dist1, probs = quantile, na.rm = TRUE)

  # Calculate resolution for the second variable
  dist2 <- stats::dist(env_data[, 2])
  res2 <- stats::quantile(dist2, probs = quantile, na.rm = TRUE)

  suggested_res <- c(res1, res2)
  names(suggested_res) <- env_vars

  if (verbose) {
    cat(sprintf("Resolution at %.2f quantile:\n", quantile))
    cat(sprintf(" - %s: %f\n", env_vars[1], suggested_res[1]))
    cat(sprintf(" - %s: %f\n", env_vars[2], suggested_res[2]))
  }

  # --- Construct S3 Object ---
  dist_df1 <- data.frame(variable = env_vars[1], distances = as.numeric(dist1))
  dist_df2 <- data.frame(variable = env_vars[2], distances = as.numeric(dist2))

  results <- list(
    suggested_resolution = suggested_res,
    distance_distributions = rbind(dist_df1, dist_df2)
  )

  class(results) <- "bean_resolution"
  return(results)
}

# Custom print and plot methods for the new class
#' @export
print.bean_resolution <- function(x, ...) {
  cat("--- Bean Environmental Resolution Analysis (Per-Axis Distance) ---\n\n")
  cat("Suggested Grid Resolutions:\n")
  cat(sprintf("  - %s: %f\n", names(x$suggested_resolution)[1], x$suggested_resolution[1]))
  cat(sprintf("  - %s: %f\n\n", names(x$suggested_resolution)[2], x$suggested_resolution[2]))
  cat("To see the full distance distributions, run plot(your_results_object).\n")
}

#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline labs theme_bw facet_wrap
plot.bean_resolution <- function(x, ...) {
  plot_data <- x$distance_distributions
  res_data <- data.frame(variable = names(x$suggested_resolution),
                         resolution = x$suggested_resolution)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = distances)) +
    ggplot2::geom_histogram(bins = 50, fill = "grey80", color = "black") +
    ggplot2::geom_vline(data = res_data, ggplot2::aes(xintercept = resolution),
                        linetype = "dashed", color = "blue", linewidth = 1) +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::labs(
      title = "Distribution of Pairwise Environmental Distances per Axis",
      subtitle = "Blue line indicates the suggested grid resolution at the specified quantile",
      x = "Environmental Distance",
      y = "Frequency"
    ) +
    ggplot2::theme_bw()
}
