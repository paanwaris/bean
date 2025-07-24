#' Perform repeated k-fold cross-validation for a Maxent model
#'
#' @description This function provides a metric for evaluating the effect of occurrences thinning in environmental space
#' using a Maxent model (see Details). Performance is tested using repeated k-fold cross-validation. The test calculates the
#' Area Under the Curve (AUC) for each fold and repetition, providing a distribution of scores
#' to assess model stability and predictive accuracy.
#'
#' @param presence_data A data.frame with at least two columns for longitude and latitude of presence points.
#' @param background_data A data.frame with at least two columns for longitude and latitude of background (or pseudo-absence) points.
#' @param env_rasters A RasterStack or SpatRaster object of environmental variables.
#' @param longitude (character) The name of the longitude column in the data.frames.
#' @param latitude (character) The name of the latitude column in the data.frames.
#' @param k (numeric) The number of folds for cross-validation. Default = 4.
#' @param n_repeats (numeric) The number of times to repeat the k-fold process. Default = 10.
#' @param maxent_args (character) A vector of arguments to be passed to the
#' "dismo::maxent" function. The default enables only linear features. See "?dismo::maxent" for details.
#' @details
#' This function serves as a wrapper for the \code{dismo::maxent} function, automating
#' the process of model evaluation through repeated k-fold cross-validation. This
#' approach provides a robust assessment of a model's predictive performance and
#' stability, which is particularly useful when comparing models built with
#' different data inputs (ten Caten et al., 2023).
#'
#' ### Maxent (\code{dismo})
#'
#' \code{\link{test_env_thinning}} relies on the \code{dismo} package, which provides an R interface to
#' the standalone Maxent Java application (Hijmans et al., 2024). Maxent is a
#' presence-background (or presence-only) machine learning algorithm that
#' estimates a species' potential distribution based on the principle of maximum
#' entropy (Phillips et al., 2006). It contrasts the environmental conditions at known
#' presence locations (\code{presence_data}) with the environmental conditions available
#' across the broader study area, as represented by the \code{background_data}.
#' The \code{maxent_args} parameter allows for direct control over model complexity, such as specifying different feature
#' classes (e.g., linear, quadratic, hinge) and adjusting the regularization
#' multiplier to prevent overfitting (Phillips et al., 2006; Warren and Seifert, 2011).
#'
#' ### Repeated K-Fold Cross-Validation
#'
#' To provide a rigorous evaluation, this function implements a repeated k-fold cross-validation procedure. The process is as follows:
#' 1. The \code{presence_data} is randomly partitioned into \code{k} equal-sized subsets (folds).
#' 2. The model is trained using \code{k-1} folds and then evaluated on the single held-out fold.
#' 3. This is repeated \code{k} times, with each fold serving as the test set exactly once.
#' 4. The entire k-fold process is then repeated \code{n_repeats} times, each time with a new random partitioning of the data.
#'
#' ### Model Evaluation using AUC
#'
#' The function calculates the Area Under the Curve (AUC) of the Receiver Operating
#' Characteristic (ROC) plot for each test fold. AUC is a threshold-independent
#' metric that measures the model's ability to discriminate between a random
#' presence site and a random background site (Phillips et al., 2006). An AUC value of 1.0
#' indicates perfect discrimination, while a value of 0.5 indicates performance
#' no better than random (Hijmans et al., 2024).
#'
#' @references
#' Hijmans R, Phillips S, Leathwick J, Elith J (2024). dismo: Species Distribution Modeling. R package version 1.3-16, <https://rspatial.org/raster/sdm/>.
#'
#' Phillips, S. J., Anderson, R. P., & Schapire, R. E. (2006). Maximum entropy modeling of species geographic distributions. Ecological modelling, 190(3-4), 231-259.
#'
#' Ten Caten, C., & Dallas, T. (2023). Thinning occurrence points does not improve species distribution model performance. Ecosphere, 14(12), e4703.
#'
#' Valavi, R., Elith, J., Lahoz-Monfort, J. J., & Guillera-Arroita, G. (2018). blockCV: An r package for generating spatially or environmentally separated folds for k-fold cross-validation of species distribution models. Biorxiv, 357798.
#'
#' Warren, D. L., & Seifert, S. N. (2011). Ecological niche modeling in Maxent: the importance of model complexity and the performance of model selection criteria. Ecological applications, 21(2), 335-342.
#'
#' @note For reproducible results, run "set.seed()" before calling this function.
#'
#' @return An object of class \code{bean_evaluation}, which is a list containing:
#'   \item{summary}{A data.frame with summary statistics (mean, sd, etc.) of the AUC scores.}
#'   \item{all_auc_scores}{A numeric vector of all AUC scores from every fold and repetition.}
#'   \item{parameters}{A list of the key parameters used in the evaluation.}
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
#' occ_file <- system.file("extdata", "Peromyscus_maniculatus_prepared.csv", package = "bean")
#' occ_data <- read.csv(occ_file)
#'
#' bio1_file <- system.file("extdata", "BIO1.tif", package = "bean")
#' bio12_file <- system.file("extdata", "BIO12.tif", package = "bean")
#' env_rasters <- raster::stack(bio1_file, bio12_file)
#'
#' # 2. Create background points
#' set.seed(81)
#' background_pts <- dismo::randomPoints(env_rasters, 1000)
#' background_df <- as.data.frame(background_pts)
#' colnames(background_df) <- c("x", "y")
#'
#' # 3. Run the evaluation with minimal settings for the example
#' auc_results <- test_env_thinning(
#'   presence_data = occ_data,
#'   background_data = background_df,
#'   env_rasters = env_rasters,
#'   longitude = "x",
#'   latitude = "y",
#'   k = 2,
#'   n_repeats = 2, # Use a small number for the example
#'   maxent_args = c("linear=true",
#'   "quadratic=true",
#'   "product=false",
#'   "threshold=false",
#'   "hinge=false",
#'   "doclamp=false")
#' )
#'
#' # 4. Print the summary and plot the distribution of AUC scores
#' print(auc_results)
#' plot(auc_results)
#' }
test_env_thinning <- function(presence_data, background_data, env_rasters,
                              longitude, latitude, k = 4, n_repeats = 10,
                              maxent_args = c("linear=true", "quadratic=false", "product=false",
                                              "threshold=false", "hinge=false", "doclamp=true")) {
  # --- Input Validation ---
  if (inherits(env_rasters, "SpatRaster")) {
    env_rasters <- raster::stack(env_rasters)
  }

  presence_coords <- presence_data[, c(longitude, latitude)]
  background_coords <- background_data[, c(longitude, latitude)]

  # --- Repeated K-Fold Cross-Validation ---
  all_auc_scores <- list()

  cat(sprintf("Starting %d repetitions of %d-fold cross-validation...\n", n_repeats, k))

  for (rep in 1:n_repeats) {
    cat(sprintf("  - Repetition %d of %d...\n", rep, n_repeats))

    # Create random folds for each repetition
    # Assumes user has set a seed externally for reproducibility
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

  cat("Validation complete.\n")

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
#' @keywords internal
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
#' Creates a diagnostic plot from the output of \code{\link{test_env_thinning}}.
#'
#' @param x An object of class \code{bean_evaluation}.
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot object.
#' @export
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density geom_vline labs theme_bw after_stat
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
