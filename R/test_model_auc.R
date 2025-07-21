#' Perform repeated k-fold cross-validation for a Maxent model
#'
#' @description This function provides a robust framework for evaluating a Maxent
#' model's performance using repeated k-fold cross-validation. It calculates the
#' Area Under the Curve (AUC) for each run, providing a distribution of scores
#' to assess model stability and predictive accuracy.
#'
#' @param presence_data A data frame with at least two columns for longitude and
#'   latitude of presence points.
#' @param background_data A data frame with at least two columns for longitude and
#'   latitude of background (or pseudo-absence) points.
#' @param env_rasters A RasterStack or SpatRaster object of environmental variables.
#' @param longitude (character) The name of the longitude column in the data frames.
#' @param latitude (character) The name of the latitude column in the data frames.
#' @param k (numeric) The number of folds for cross-validation. Default = 4.
#' @param n_repeats (numeric) The number of times to repeat the k-fold process.
#'   Default = 10.
#' @param maxent_args (character) A vector of arguments to be passed to the
#'   `dismo::maxent` function. See `?dismo::maxent` for details.
#' @param verbose (logical) If TRUE, prints progress messages. Default = TRUE.
#'
#' @return An object of class \code{bean_evaluation}.
#'
#' @export
#' @importFrom dismo kfold maxent evaluate
#' @importFrom raster stack
#' @examples
#' \dontrun{
#' # This is a long-running example and requires Maxent to be installed.
#'
#' # 1. Load the package's example data
#' library(raster)
#' occ_file <- system.file("extdata", "P_maniculatus_samples.csv", package = "bean")
#' occ_data <- read.csv(occ_file)
#'
#' bio1_file <- system.file("extdata", "climate", "BIO1.tif", package = "bean")
#' bio12_file <- system.file("extdata", "climate", "BIO12.tif", package = "bean")
#' env_rasters <- raster::stack(bio1_file, bio12_file)
#'
#' # 2. Create background points
#' set.seed(1)
#' background_pts <- dismo::randomPoints(env_rasters, 1000)
#' background_df <- as.data.frame(background_pts)
#' colnames(background_df) <- c("x", "y")
#'
#' # 3. Run the evaluation with minimal settings for the example
#' auc_results <- test_model_auc(
#'   presence_data = occ_data,
#'   background_data = background_df,
#'   env_rasters = env_rasters,
#'   longitude = "x",
#'   latitude = "y",
#'   k = 2,
#'   n_repeats = 2, # Use a small number for the example
#'     maxent_args = c("linear=true",
#'     "quadratic=true",
#'     "product=false",
#'     "threshold=false",
#'     "hinge=false",
#'     "doclamp=true")
#' )
#'
#' # 4. Print the summary and plot the distribution of AUC scores
#' print(auc_results)
#' plot(auc_results)
#' }
test_model_auc <- function(presence_data, background_data, env_rasters,
                           longitude, latitude, k = 4, n_repeats = 10,
                           maxent_args = c("linear=true", "quadratic=true", "product=false",
                                           "threshold=false", "hinge=false", "doclamp=true"),
                           verbose = TRUE) {
  # --- Input Validation ---
  if (inherits(env_rasters, "SpatRaster")) {
    env_rasters <- raster::stack(env_rasters)
  }

  presence_coords <- presence_data[, c(longitude, latitude)]
  background_coords <- background_data[, c(longitude, latitude)]

  # --- Repeated K-Fold Cross-Validation ---
  all_auc_scores <- list()

  if (verbose) cat(sprintf("Starting %d repetitions of %d-fold cross-validation...\n", n_repeats, k))

  for (rep in 1:n_repeats) {
    if (verbose) cat(sprintf("  - Repetition %d of %d...\n", rep, n_repeats))

    # Create NEW random folds for each repetition
    set.seed(Sys.time()) # Use a different seed for each set of folds
    folds <- dismo::kfold(presence_coords, k = k)

    auc_scores_for_this_rep <- numeric(k)

    for (i in 1:k) {
      train_p <- presence_coords[folds != i, ]
      test_p  <- presence_coords[folds == i, ]

      mx_model <- dismo::maxent(
        x = env_rasters,
        p = train_p,
        a = background_coords,
        args = maxent_args
      )

      model_eval <- dismo::evaluate(p = test_p, a = background_coords, model = mx_model, x = env_rasters)
      auc_scores_for_this_rep[i] <- model_eval@auc
    }

    all_auc_scores[[rep]] <- auc_scores_for_this_rep
  }

  if (verbose) cat("Validation complete.\n")

  # --- Construct S3 Object ---
  auc_vector <- unlist(all_auc_scores)

  summary_stats <- data.frame(
    Mean_AUC = mean(auc_vector),
    SD_AUC = sd(auc_vector),
    Median_AUC = median(auc_vector),
    Min_AUC = min(auc_vector),
    Max_AUC = max(auc_vector)
  )

  results <- list(
    summary = summary_stats,
    all_auc_scores = auc_vector,
    parameters = list(k = k, n_repeats = n_repeats, maxent_args = maxent_args)
  )

  class(results) <- "bean_evaluation"
  return(results)
}

#' @export
print.bean_evaluation <- function(x, ...) {
  cat("--- Bean Model Evaluation Results ---\n\n")
  cat(sprintf("Based on %d repetitions of %d-fold cross-validation (%d total models).\n\n",
              x$parameters$n_repeats, x$parameters$k, length(x$all_auc_scores)))

  cat("Summary of AUC Scores:\n")
  print(round(x$summary, 3))

  cat("\nTo see the distribution of AUC scores, run plot(your_results_object).\n")
}
#' Plot bean_evaluation results
#'
#' Creates a diagnostic plot from the output of \code{\link{test_model_auc}}.
#'
#' @param x An object of class \code{bean_evaluation}.
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density geom_vline labs theme_bw
plot.bean_evaluation <- function(x, ...) {
  plot_data <- data.frame(auc = x$all_auc_scores)

  model_auc_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = auc)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 20, fill = "grey80", color = "black", alpha = 0.7) +
    ggplot2::geom_density(color = "blue", linewidth = 1) +
    ggplot2::geom_vline(xintercept = x$summary$Mean_AUC, color = "red", linetype = "dashed", linewidth = 1) +
    ggplot2::labs(
      title = "Distribution of Cross-Validation AUC Scores",
      subtitle = sprintf("Red dashed line indicates the mean AUC (%.3f)", x$summary$Mean_AUC),
      x = "AUC Score",
      y = "Density"
    ) +
    ggplot2::theme_bw()
  return(model_auc_plot)
}
