#' Find objective grid resolutions for each environmental axis
#'
#' @description This function calculates an objective, data-driven grid resolution for use in
#' environmental thinning. It operates by analyzing the distribution of pairwise
#' 1D distances for each environmental variable separately. The distance at a
#' specified lower quantile of this distribution is returned as a suggested
#' grid resolution for each axis. The function allows for the creation of rectangular
#' grid cells that are adapted to the unique scale and clustering within each
#' environmental dimension.
#'
#' @param data A data.frame containing species occurrence coordinates and the environmental variables.
#' @param env_vars A character vector specifying the column names in data that represent the environmental variables to be used in the analysis.
#' @param quantile (numeric) The quantile of pairwise distances to use for the
#'   resolution. A smaller quantile value (e.g., 0.05-0.25) is generally recommended for gentle
#'   thinning. However, for widespread generalists, a higher quantile (e.g., > 0.75) may improve
#'   model performance by more aggressively reducing large-scale bias. Default = 0.1.
#'
#' @return An object of class \code{bean_resolution}, which is a list containing:
#'   \item{suggested_resolution}{A named numeric vector of the calculated grid
#'     resolutions for each environmental variable.}
#'   \item{distance_distributions}{A data frame containing all pairwise
#'     distances for each variable, suitable for plotting.}
#'   \item{quantile}{The percentile used for the calculation.}
#'
#' @details
#' ### The Rationale for Using a Distance Quantile (\code{quantile})
#'
#' Selecting an appropriate grid resolution for environmental thinning is crucial
#' for reducing sampling bias without discarding excessive data. Rather than
#' relying on an arbitrary, user-defined value, this function employs a
#' data-driven heuristic to determine a characteristic scale from the data
#' itself based on environmental filtering (Varela et al., 2014). For each environmental variable,
#' it calculates all pairwise distances between occurrence points. The quantile of this
#' distribution is then used as the grid resolution. This approach adapts the cell size to the
#' inherent data structure and focuses on distributional properties rather than just the mean (Cade & Noon, 2003).
#'
#' ### Selecting a Quantile
#'
#' The choice of (\code{quantile}) allows you to control the aggressiveness of the
#' thinning. It creates a spectrum from gentle, fine-scale thinning (low quantiles)
#' to more aggressive, large-scale bias reduction (high quantiles). The optimal
#' value often depends on the species' ecology and the nature of the sampling bias.
#'
#' - Low Quantiles (e.g., 0.05-0.25): A low quantile selects a *small
#'   distance, creating a fine grid. This performs a gentle thinning that removes
#'   fine-scale clustering while preserving the overall data structure. This is
#'   often a suitable starting point, especially for species with more specialized
#'   niches.
#'
#' - High Quantiles (e.g., > 0.75): The primary issue is often large-scale, regional sampling bias. In
#'   these cases, a high quantile can improve model performance by selecting a
#'   large distance. This creates a coarse grid that aggressively
#'   de-clusters the data, forcing the model to learn the broad, fundamental
#'   tolerances of the species.
#'
#' @note The quantile-based approach has the advantage of adapting the thinning
#' resolution to the data's inherent structure. Because there is no universal
#' best value, the recommended approach is to use the \code{\link{calibrate_bean}}
#' function to test a range of quantiles and let the model's final performance
#' guide the selection.
#' @seealso \code{\link{calibrate_bean}}
#' @references
#' Cade, B. S., & Noon, B. R. (2003). A gentle introduction to quantile regression for ecologists. Frontiers in Ecology and the Environment, 1(8), 412–420.
#'
#' Varela, S., Anderson, R. P., García-Valdés, R., & Fernández-González, F. (2014). Environmental filters reduce the effects of sampling bias and improve predictions of ecological niche models. Ecography, 37(11), 1084–1091.
#' @export
#' @importFrom stats dist quantile
#' @importFrom tidyr pivot_longer
#' @examples
#' \dontrun{
#' # 1. Create the example occurrence records and environmental variables
#' occ_data <- data.frame(
#'  BIO1 = c(0, 1, 3, 6),
#'  BIO12 = c(0, 10, 30, 60),
#'  x = c(1, 1, 0, 0.5),
#'  y = c(0, 0.5, 2, 1.5),
#'  species = "A"
#' )
#
#' # 2. Find the resolution at the 10th percentile
#' resolutions <- find_env_resolution(
#'   data = occ_data,
#'   env_vars = c("BIO1", "BIO12"),
#'   quantile = 0.1
#' )
#'
#' # 3. Print the summary and plot the results
#' print(resolutions)
#' plot(resolutions)
#' }
find_env_resolution <- function(data, env_vars, quantile = 0.1) {
  # --- Input Validation and Data Cleaning ---
  if (length(env_vars) < 2) {
    stop("`env_vars` must contain at least two variable names")
  }
  if (!all(env_vars %in% names(data))) {
    stop("One or more `env_vars` not found in the data frame")
  }

  clean_data <- data[stats::complete.cases(data[, env_vars]), ]

  if (nrow(clean_data) < 3) {
    stop("At least 3 complete observations are needed to calculate resolution")
  }

  # Data is assumed to be pre-scaled
  env_data <- clean_data[, env_vars, drop = FALSE]

  # --- Calculate Resolutions for Each Axis ---
  cat("Calculating pairwise distances for each environmental axis...\n")

  # Use lapply to iterate over each environmental variable
  resolution_list <- lapply(env_vars, function(var) {
    distances <- stats::dist(env_data[[var]])
    resolution <- stats::quantile(distances, probs = quantile, na.rm = TRUE)
    return(list(
      resolution = resolution,
      distances = as.numeric(distances)
    ))
  })

  # --- Construct S3 Object ---
  suggested_res <- sapply(resolution_list, `[[`, "resolution")
  names(suggested_res) <- env_vars

  distance_df_list <- lapply(seq_along(env_vars), function(i) {
    data.frame(
      variable = env_vars[i],
      distances = resolution_list[[i]]$distances
    )
  })

  distance_distributions <- do.call(rbind, distance_df_list)

  results <- list(
    suggested_resolution = suggested_res,
    distance_distributions = distance_distributions,
    parameters = list(quantile = quantile)
  )

  class(results) <- "bean_resolution"
  return(results)
}

#' @export
#' @keywords internal
print.bean_resolution <- function(x, ...) {
  cat("--- Bean Environmental Resolution Analysis ---\n\n")
  cat(sprintf("Suggested Grid Resolutions (at the %.0f%% quantile):\n", x$parameters$quantile * 100))

  # Loop through all resolutions to print them
  for (i in seq_along(x$suggested_resolution)) {
    cat(sprintf("  - %s: %f\n", names(x$suggested_resolution)[i], x$suggested_resolution[i]))
  }

  cat("\nTo see the full distance distributions, run plot(your_results_object).\n")
}

#' @export
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline labs theme_bw facet_wrap
plot.bean_resolution <- function(x, ...) {
  plot_data <- x$distance_distributions
  res_data <- data.frame(
    variable = names(x$suggested_resolution),
    resolution = x$suggested_resolution
  )

  res_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = distances)) +
    ggplot2::geom_histogram(bins = 50, fill = "grey80", color = "black") +
    ggplot2::geom_vline(
      data = res_data, ggplot2::aes(xintercept = resolution),
      linetype = "dashed", color = "blue", linewidth = 1
    ) +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::labs(
      title = "Distribution of Pairwise Environmental Distances per Axis",
      subtitle = sprintf("Blue line indicates the suggested grid resolution at the %.0fth percentile", x$parameters$quantile * 100),
      x = "Environmental Distance (scaled)",
      y = "Frequency"
    ) +
    ggplot2::theme_bw()

  return(res_plot)
}
