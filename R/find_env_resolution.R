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
#'   precision. A smaller value (e.g., 0.05 or 0.1) is recommended to retain most of the original points and it represents the spacing between closely clustered points.
#'   Default = 0.1 (10th percentile). See Details.
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
#' itself based on environmental filtering (Varela et al., 2014).
#'
#' For each environmental variable, the function calculates all pairwise
#' distances between the occurrence points. This creates a distribution that
#' reflects the internal spacing and clustering of the data in that one
#' dimension. By selecting a low quantile of this distribution (e.g.,
#' "quantile = 0.1" for the 10th percentile is recommended), we identify a distance that is
#' representative of the spacing between closely clustered points.
#' This approach is conceptually powerful because it focuses on the
#' distributional edges to identify limiting factors or characteristic scales,
#' rather than being limited to an analysis of the mean (Cade & Noon, 2003).
#'
#' Using the grid resolution from the quantile approach is a robust strategy because it
#' adapts the cell size to the inherent data structure. If points are tightly
#' clustered in one dimension (e.g., a species has a narrow thermal tolerance),
#' the resulting resolution will be fine. If points are spread out, the
#' resolution will be coarser.
#'
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
  if (!all(env_vars %in% names(data))) {
    stop("One or both specified env_vars not found in the data frame.")
  }

  env_var1_sym <- rlang::sym(env_vars[1])
  env_var2_sym <- rlang::sym(env_vars[2])

  clean_data <- data %>%
    dplyr::filter(is.finite(!!env_var1_sym) & is.finite(!!env_var2_sym))

  if (nrow(clean_data) < 3) {
    stop("At least 3 complete observations are needed to calculate resolution.")
  }

  # Data is assumed to be pre-scaled, so we select the columns directly
  env_data <- clean_data[, env_vars]

  # --- Calculate Resolutions for Each Axis ---
  cat("Calculating pairwise distances for each environmental axis...\n")

  dist1 <- stats::dist(env_data[, 1])
  res1 <- stats::quantile(dist1, probs = quantile, na.rm = TRUE)

  dist2 <- stats::dist(env_data[, 2])
  res2 <- stats::quantile(dist2, probs = quantile, na.rm = TRUE)

  suggested_res <- c(res1, res2)
  names(suggested_res) <- env_vars

  # --- Construct S3 Object ---
  dist_df1 <- data.frame(variable = env_vars[1], distances = as.numeric(dist1))
  dist_df2 <- data.frame(variable = env_vars[2], distances = as.numeric(dist2))

  results <- list(
    suggested_resolution = suggested_res,
    distance_distributions = rbind(dist_df1, dist_df2),
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
  cat(sprintf("  - %s: %f\n", names(x$suggested_resolution)[1], x$suggested_resolution[1]))
  cat(sprintf("  - %s: %f\n\n", names(x$suggested_resolution)[2], x$suggested_resolution[2]))
  cat("To see the full distance distributions, run plot(your_results_object).\n")
}

#' @export
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline labs theme_bw facet_wrap
plot.bean_resolution <- function(x, ...) {
  plot_data <- x$distance_distributions
  res_data <- data.frame(variable = names(x$suggested_resolution),
                         resolution = x$suggested_resolution)

  res_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = distances)) +
    ggplot2::geom_histogram(bins = 50, fill = "grey80", color = "black") +
    ggplot2::geom_vline(data = res_data, ggplot2::aes(xintercept = resolution),
                        linetype = "dashed", color = "blue", linewidth = 1) +
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
