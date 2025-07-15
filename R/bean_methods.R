#' bean_optimization object
#'
#' An object of class \code{bean_optimization} is a list returned by the
#' \code{\link{find_optimal_cap}} function. It contains the results of the
#' optimization search.
#'
#' @name bean_optimization
#' @docType class
#' @keywords internal
NULL
#' @export
print.bean_optimization <- function(x, ...) {
  cat("--- Bean Optimization Results ---\n\n")
  cat("Recommendation for 'Closest to Target':\n")
  cat(sprintf("  - Best Cap: %d\n", x$best_cap_closest))
  cat(sprintf("  - Retained Points: %d\n\n", x$retained_points_closest))

  cat("Recommendation for 'Closest Above Target':\n")
  if (is.na(x$best_cap_above_target)) {
    cat("  - No cap found that was at or above the target percentage.\n")
  } else {
    cat(sprintf("  - Best Cap: %d\n", x$best_cap_above_target))
    cat(sprintf("  - Retained Points: %d\n\n", x$retained_points_above_target))
  }

  cat("To see the diagnostic plot, run plot(your_results_object).\n")
}
#' Plot bean_optimization results
#'
#' Creates a diagnostic plot from the output of \code{\link{find_optimal_cap}}.
#'
#' @param x An object of class \code{bean_optimization}.
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_vline geom_hline labs theme_bw
plot.bean_optimization <- function(x, ...) {
  # --- Create Plot ---
  cap_plot <- ggplot2::ggplot(x$search_results, ggplot2::aes(x = cap, y = thinned_count)) +
    ggplot2::geom_line(color = "gray50") +
    ggplot2::geom_point(color = "black") +
    ggplot2::geom_hline(yintercept = x$parameters$target_point_count, linetype = "dashed", color = "red") +
    ggplot2::geom_vline(xintercept = x$best_cap_closest, linetype = "dashed", color = "blue") +
    ggplot2::labs(
      title = "Search for Optimal Density Cap",
      x = "Maximum Points per Cell (Cap)",
      y = "Number of Points Retained",
      caption = paste0(
        "Red line: Target count (", x$parameters$target_point_count, ")\n",
        "Blue line: 'Closest' cap (", x$best_cap_closest, ")"
      )
    ) +
    ggplot2::theme_bw()

  return(cap_plot)
}
