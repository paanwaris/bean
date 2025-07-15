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
