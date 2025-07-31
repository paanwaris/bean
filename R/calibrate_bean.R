#' Calibrate thinning and niche parameters using model performance
#'
#' @description This function automates the selection of an optimal set of
#' parameters for the entire \code{bean} workflow. It treats the \code{quantile} for
#' resolution, the \code{target_percent} for thinning, and the \code{method} for
#' ellipsoid fitting as model hyperparameters. It iterates through a range of
#' these values, performs the full \code{prepare -> resolve -> thin -> fit} workflow,
#' and evaluates the final model's performance using repeated k-fold
#' cross-validation by calculating the Area Under the Curve (AUC). The function identifies the combination of parameters
#' that produces the highest mean AUC and provides statistical comparisons against
#' a baseline model built with unthinned data, using an ANOVA and a Tukey's HSD
#' post-hoc test.
#'
#' @param data A data.frame containing species occurrences and pre-scaled
#'   environmental variables, typically the output of \code{\link{prepare_bean}}.
#' @param env_vars A character vector specifying the column names in data that
#'   represent the environmental variables.
#' @param background_data A data.frame with longitude and latitude of background points.
#' @param env_rasters A RasterStack or SpatRaster object of environmental variables.
#' @param longitude (character) The name of the longitude column in the data frames.
#' @param latitude (character) The name of the latitude column in the data frames.
#' @param quantile_range A numeric vector of quantile values to test for resolution finding.
#' @param method_range A character vector of ellipsoid methods to test (e.g., "c("covmat", "mve")").
#' @param target_percent (numeric) The target proportion of points to retain after density thinning.
#' @param level (numeric) A single value between 0 and 1 representing the
#'   proportion of data points the ellipse is intended to encompass.
#'   Default is 0.95.
#' @param thinning_reps (numeric) The number of times to repeat the stochastic
#'   thinning process for each parameter combination to get a stable performance estimate.
#' @param k (numeric) The number of folds for cross-validation. Default = 4.
#' @param n_repeats (numeric) The number of times to repeat the k-fold process. Default = 10.
#' @param maxent_args (character) A vector of arguments for \code{dismo::maxent}.
#'
#' @note This is a very computationally intensive function. For reproducible
#' results, run \code{set.seed()} before calling this function.
#'
#' @seealso \code{\link{prepare_bean}}
#'
#' @return An object of class \code{bean_calibration}, which is a list containing:
#'   \item{best_parameters}{A list with the optimal "quantile", "method", "resolution", and "cap".}
#'   \item{calibration_summary}{A tibble summarizing the performance for each tested combination, including statistical comparisons to the baseline.}
#'   \item{baseline_auc}{A numeric vector of the AUC scores from the model built on the original, unthinned data.}
#'   \item{best_points_in_ellipse}{The data.frame of points inside the ellipse from the best performing model.}
#'   \item{best_points_outside_ellipse}{The data.frame of points outside the ellipse from the best performing model.}
#'   \item{parameters}{A list containing the parameter ranges used in the calibration (e.g., \code{quantile_range}).}
#'
#' @export
#' @importFrom dplyr tibble add_row slice_max arrange filter mutate left_join group_by summarise select bind_rows desc
#' @importFrom tidyr expand_grid separate
#' @importFrom stats aov TukeyHSD var
#' @examples
#' \dontrun{
#' # This is a long-running example and requires the "dismo" package to be installed.
#'
#' # Load the package's example data
#' library(raster)
#' library(dismo)
#' occ_file <- system.file("extdata", "Peromyscus_maniculatus_prepared.csv", package = "bean")
#' occ_data <- read.csv(occ_file)
#'
#' bio1_file <- system.file("extdata", "BIO1.tif", package = "bean")
#' bio12_file <- system.file("extdata", "BIO12.tif", package = "bean")
#' env_rasters <- raster::stack(bio1_file, bio12_file)
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Calibrate all key parameters
#' final_calibration <- calibrate_bean(
#'   data = occ_data,
#'   env_vars = c("BIO1", "BIO12"),
#'   background_data = background_df,
#'   env_rasters = env_rasters,
#'   longitude = "x",
#'   latitude = "y",
#'   quantile_range = seq(0.1, 0.9, 0.1),
#'   method_range = c("covmat", "mve"),
#'   target_percent = 0.95,
#'   level = 0.95,
#'   thinning_reps = 3, # Use a small number for the example
#'   k = 2,
#'   n_repeats = 2 # Use minimal settings for a runnable example
#' )
#'
#' # Print the summary to see the best combination of parameters
#' print(final_calibration)
#'
#' # Access the final, best-thinned data directly
#' final_data <- final_calibration$best_points_in_ellipse
#' head(final_data)
#' }
calibrate_bean <- function(data, env_vars, background_data, env_rasters,
                           longitude, latitude,
                           quantile_range = seq(0.1, 0.9, 0.1),
                           method_range = c("covmat", "mve"),
                           target_percent = 0.95,
                           level = 0.95,
                           thinning_reps = 10,
                           k = 4, n_repeats = 10,
                           maxent_args = c("linear=true", "quadratic=false", "product=false",
                                           "threshold=false", "hinge=false", "doclamp=true")) {

  # --- Helper for significance stars ---
  get_sig_stars <- function(p) {
    sapply(p, function(val) {
      if (is.na(val)) return("NA")
      if (val < 0.001) return("***")
      if (val < 0.01) return("**")
      if (val < 0.05) return("*")
      return("ns")
    })
  }

  # --- 1. Evaluate Baseline Models ---
  all_auc_data <- list()

  cat("--- Evaluating Baseline Model (Original Data) ---\n")
  baseline_eval <- test_env_thinning(
    presence_data = data, background_data = background_data, env_rasters = env_rasters,
    longitude = longitude, latitude = latitude, k = k, n_repeats = n_repeats, maxent_args = maxent_args
  )
  all_auc_data$Original <- baseline_eval$all_auc_scores

  for (m in method_range) {
    combo_name <- paste0("Original_", m)
    cat(sprintf("--- Evaluating Baseline Model (%s) ---\n", combo_name))
    ellipse_obj <- fit_ellipsoid(data = data, env_vars = env_vars, method = m, level = level)

    baseline_ellipsoid_eval <- test_env_thinning(
      presence_data = ellipse_obj$points_in_ellipse, background_data = background_data, env_rasters = env_rasters,
      longitude = longitude, latitude = latitude, k = k, n_repeats = n_repeats, maxent_args = maxent_args
    )
    all_auc_data[[combo_name]] <- baseline_ellipsoid_eval$all_auc_scores
  }

  # --- 2. Calibrate Thinning Parameters ---
  param_grid <- tidyr::expand_grid(quantile = quantile_range, method = method_range)
  cat(sprintf("\nStarting calibration across %d parameter combinations...\n", nrow(param_grid)))

  for (row_index in 1:nrow(param_grid)) {
    q <- param_grid$quantile[row_index]
    m <- param_grid$method[row_index]
    combo_name <- sprintf("q%.2f_%s", q, m)
    cat(sprintf("\n--- Testing Combination %d/%d: %s ---\n", row_index, nrow(param_grid), combo_name))

    resolution_obj <- find_env_resolution(data = data, env_vars = env_vars, quantile = q)
    current_res <- resolution_obj$suggested_resolution
    cap_obj <- find_optimal_cap(data = data, env_vars = env_vars, grid_resolution = current_res, target_percent = target_percent)
    current_cap <- cap_obj$best_cap_above_target

    if (is.na(current_cap)) {
      message("No suitable cap found, skipping this combination.")
      all_auc_data[[combo_name]] <- NA
      next
    }

    all_rep_auc_scores <- c()
    for (rep in 1:thinning_reps) {
      cat(sprintf("    - Thinning Repetition %d/%d...\n", rep, thinning_reps))
      thinned_obj <- thin_env_nd(data = data, env_vars = env_vars, grid_resolution = current_res, max_per_cell = current_cap)
      ellipse_obj <- fit_ellipsoid(data = thinned_obj$thinned_data, env_vars = env_vars, method = m, level = level)

      final_points <- ellipse_obj$points_in_ellipse

      if (nrow(final_points) < 10) {
        message("Too few points remaining after thinning/fitting (< 10), skipping this rep.")
        next
      }
      env_variances <- sapply(final_points[, env_vars, drop = FALSE], stats::var, na.rm = TRUE)
      if (any(is.na(env_variances)) || any(env_variances == 0)) {
        message("Skipping rep due to zero variance in an environmental variable after thinning.")
        next
      }

      eval_obj <- test_env_thinning(
        presence_data = as.data.frame(final_points),
        background_data = background_data, env_rasters = env_rasters,
        longitude = longitude, latitude = latitude,
        k = k, n_repeats = n_repeats, maxent_args = maxent_args
      )
      all_rep_auc_scores <- c(all_rep_auc_scores, eval_obj$all_auc_scores)
    }

    # Handle cases where all reps failed for a combination
    if (length(all_rep_auc_scores) > 0) {
      all_auc_data[[combo_name]] <- all_rep_auc_scores
    } else {
      all_auc_data[[combo_name]] <- NA
    }
  }

  # --- 3. Perform Statistical Analysis ---
  cat("\n--- Performing Statistical Comparison ---\n")

  # Robustly create data frame for analysis, ignoring failed combinations
  auc_df <- dplyr::bind_rows(lapply(names(all_auc_data), function(name) {
    if (is.numeric(all_auc_data[[name]])) {
      return(dplyr::tibble(auc = all_auc_data[[name]], combination = name))
    }
    return(NULL)
  }))

  calibration_summary <- auc_df %>%
    dplyr::group_by(combination) %>%
    dplyr::summarise(mean_auc = mean(auc, na.rm = TRUE), sd_auc = sd(auc, na.rm = TRUE), .groups = "drop")

  if (length(unique(auc_df$combination)) < 2) {
    message("Fewer than two valid groups to compare; skipping statistical tests.")
    calibration_summary$p_value_vs_original <- NA
    calibration_summary$significance <- NA
    calibration_summary$group <- NA
  } else {
    aov_results <- stats::aov(auc ~ combination, data = auc_df)
    tukey_results <- stats::TukeyHSD(aov_results)

    p_values_df <- as.data.frame(tukey_results$combination)
    p_values_df$comparison <- rownames(p_values_df)
    p_values_df <- p_values_df[grep("-Original$", p_values_df$comparison), ]
    p_values_df$combination <- gsub("-Original", "", p_values_df$comparison)

    tukey_pvals <- tukey_results$combination[, "p adj"]
    tukey_groups <- multcompView::multcompLetters(tukey_pvals)$Letters
    group_df <- data.frame(combination = names(tukey_groups), group = tukey_groups)

    calibration_summary <- calibration_summary %>%
      dplyr::left_join(p_values_df[, c("combination", "p adj")], by = "combination") %>%
      dplyr::rename(p_value_vs_original = `p adj`) %>%
      dplyr::mutate(significance = get_sig_stars(p_value_vs_original)) %>%
      dplyr::left_join(group_df, by = "combination")
  }

  calibration_summary <- calibration_summary %>% dplyr::arrange(dplyr::desc(mean_auc))

  # --- 4. Find and return the best result ---
  best_result <- calibration_summary %>%
    dplyr::filter(!grepl("Original", combination) & !is.na(mean_auc)) %>%
    dplyr::slice_max(order_by = mean_auc, n = 1, with_ties = FALSE)

  if (nrow(best_result) == 0) {
    stop("Calibration failed to produce any valid thinned models.")
  }

  best_q <- as.numeric(gsub("q|_.*", "", best_result$combination))
  best_m <- gsub(".*_", "", best_result$combination)

  # --- 5. Re-run the best combination to get the final data products ---
  cat("\n--- Generating final data products for the best parameter combination ---\n")
  final_res_obj <- find_env_resolution(data = data, env_vars = env_vars, quantile = best_q)
  final_cap_obj <- find_optimal_cap(data = data, env_vars = env_vars, grid_resolution = final_res_obj$suggested_resolution, target_percent = target_percent)
  final_thinned_obj <- thin_env_nd(data = data, env_vars = env_vars, grid_resolution = final_res_obj$suggested_resolution, max_per_cell = final_cap_obj$best_cap_above_target)
  final_ellipse_obj <- fit_ellipsoid(data = final_thinned_obj$thinned_data, env_vars = env_vars, method = best_m, level = level)

  results <- list(
    parameters = list(quantile_range = quantile_range),
    best_parameters = list(
      quantile = best_q,
      method = best_m,
      resolution = final_res_obj$suggested_resolution,
      cap = final_cap_obj$best_cap_above_target
    ),
    calibration_summary = calibration_summary,
    baseline_auc = all_auc_data,
    best_points_in_ellipse = final_ellipse_obj$points_in_ellipse,
    best_points_outside_ellipse = final_ellipse_obj$points_outside_ellipse
  )

  class(results) <- "bean_calibration"
  return(results)
}

#' @export
#' @keywords internal
print.bean_calibration <- function(x, ...) {
  cat("--- Bean Parameter Calibration Results ---\n\n")
  cat("Search Summary (sorted by performance):\n")

  print_summary <- x$calibration_summary %>%
    dplyr::mutate(
      mean_auc = round(mean_auc, 3),
      sd_auc = round(sd_auc, 3),
      p_value_vs_original = format.pval(p_value_vs_original, digits = 2)
    ) %>%
    dplyr::select(combination, mean_auc, sd_auc, p_value_vs_original, significance, group)

  print(print_summary, n = Inf)

  cat("\n--- Best Combination ---\n")
  best <- x$best_parameters
  cat(sprintf("Optimal Quantile: %.3f\n", best$quantile))
  cat(sprintf("Optimal Ellipse Method: '%s'\n", best$method))
  cat("Resulting Grid Resolution:\n")

  # FIX: Loop through all resolutions instead of printing only the first two
  if (!is.null(best$resolution) && length(best$resolution) > 0) {
    for (i in seq_along(best$resolution)) {
      cat(sprintf("  - %s: %.4f\n", names(best$resolution)[i], best$resolution[i]))
    }
  }

  cat(sprintf("Resulting Thinning Cap: %d\n", best$cap))

  cat("---\n")
  cat("Significance stars (*) indicate p-value from a pairwise comparison against the 'Original' baseline model.\n")
  cat("Signif. codes: '***' p < 0.001,  '**' p < 0.01,  '*' p < 0.05,  'ns' p >= 0.05\n")
  cat("Models sharing a letter in the 'group' column are not significantly different from each other (Tukey's HSD).\n")
}


#' @export
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_errorbar labs theme_bw facet_wrap geom_hline geom_rect scale_x_continuous position_dodge coord_cartesian geom_text
plot.bean_calibration <- function(x, ...) {
  plot_data <- x$calibration_summary %>% dplyr::filter(!grepl("Original", combination))
  baseline_summaries <- x$calibration_summary %>% dplyr::filter(grepl("Original", combination))

  if (nrow(plot_data) == 0) {
    warning("No valid thinned models to plot.")
    return(invisible(NULL))
  }

  plot_data <- plot_data %>%
    tidyr::separate(combination, into = c("quantile", "method"), sep = "_", remove = FALSE) %>%
    dplyr::mutate(quantile = as.numeric(gsub("q", "", quantile)))

  best_data <- plot_data %>%
    dplyr::filter(quantile == x$best_parameters$quantile, method == x$best_parameters$method)

  # --- Calculate dynamic y-axis limits ---
  full_summary <- x$calibration_summary
  y_min <- min(full_summary$mean_auc - full_summary$sd_auc, na.rm = TRUE)
  y_max <- max(full_summary$mean_auc + full_summary$sd_auc, na.rm = TRUE)
  y_buffer <- (y_max - y_min) * 0.1 # Increase buffer for text labels

  dodge_width <- 0
  if (length(unique(plot_data$quantile)) > 1) {
    dodge_width <- min(diff(sort(unique(plot_data$quantile)))) * 0.4
  }

  calib_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = quantile, y = mean_auc, color = method))

  # --- Add Baseline Lines (Mean Only) ---
  baseline_colors <- c("Original" = "black", "Original_covmat" = "blue", "Original_mve" = "purple")
  for(i in 1:nrow(baseline_summaries)) {
    b_summary <- baseline_summaries[i, ]
    if (!is.na(b_summary$mean_auc)) {
      calib_plot <- calib_plot +
        ggplot2::geom_hline(yintercept = b_summary$mean_auc, linetype = "dashed", color = baseline_colors[b_summary$combination], linewidth = 0.5)
    }
  }

  # --- Add Calibration Results with Error Bars ---
  calib_plot <- calib_plot +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_auc - sd_auc, ymax = mean_auc + sd_auc),
      width = 0.02, alpha = 0.8,
      position = ggplot2::position_dodge(width = dodge_width)
    ) +
    ggplot2::geom_line(alpha = 0.8, position = ggplot2::position_dodge(width = dodge_width)) +
    ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = dodge_width)) +
    ggplot2::scale_x_continuous(breaks = x$parameters$quantile_range) +
    ggplot2::coord_cartesian(ylim = c(y_min - y_buffer, y_max + y_buffer)) +
    ggplot2::labs(
      title = "Model Performance vs. Thinning Parameters",
      subtitle = "Dashed lines are baselines: Black=Unthinned, Blue=Baseline covmat, Purple=Baseline mve",
      x = "Distance Quantile Used for Grid Resolution",
      y = "Area Under the Curve (AUC)",
      color = "Ellipse Method"
    ) +
    ggplot2::theme_bw()

  return(calib_plot)
}
