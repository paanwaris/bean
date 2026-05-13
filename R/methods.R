#' Predict suitability and Mahalanobis distance from a bean ellipsoid
#'
#' @description
#' Computes Mahalanobis distance and suitability values deriving from a fitted
#' \code{bean_ellipsoid} object for new environmental data.
#'
#' @param object An object of class \code{bean_ellipsoid}.
#' @param newdata Environmental predictors. Can be a \code{data.frame}, \code{matrix},
#'   or a \code{terra::SpatRaster}. Must contain column/layer names matching the
#'   variables used to fit the ellipsoid.
#' @param include_suitability (logical) If \code{TRUE} (default), returns continuous
#'   suitability values.
#' @param suitability_truncated (logical) If \code{TRUE}, returns a truncated
#'   suitability layer where values outside the chi-square contour are set to \code{0}.
#'   Default = \code{FALSE}.
#' @param include_mahalanobis (logical) If \code{TRUE} (default), returns continuous
#'   Mahalanobis distance.
#' @param mahalanobis_truncated (logical) If \code{TRUE}, returns a truncated
#'   Mahalanobis layer where values outside the chi-square contour are set to \code{NA}.
#'   Default = \code{FALSE}.
#' @param keep_data (logical) If \code{TRUE}, includes the original predictors in
#'   the output. Default is \code{FALSE} for SpatRaster and \code{TRUE} for data.frames.
#' @param ... Additional arguments (unused).
#'
#' @return A \code{data.frame} or \code{SpatRaster} (matching the input type of \code{newdata})
#'   containing the requested prediction layers.
#'
#' @export
#' @method predict bean_ellipsoid
#' @importFrom stats mahalanobis qchisq complete.cases
predict.bean_ellipsoid <- function(object, newdata,
                                   include_suitability = TRUE,
                                   suitability_truncated = FALSE,
                                   include_mahalanobis = TRUE,
                                   mahalanobis_truncated = FALSE,
                                   keep_data = NULL, ...) {

  # --- 1. Identify Variables and Cutoff Threshold ---
  env_vars <- names(object$centroid)
  n_dim <- length(env_vars)

  # Inherit the exact confidence level from the fitted model
  cl <- object$parameters$level
  chi_cutoff <- stats::qchisq(cl, df = n_dim)

  # --- 2. Determine Input Type ---
  is_raster <- inherits(newdata, "SpatRaster")

  if (is.null(keep_data)) {
    keep_data <- !is_raster # Default: keep data for df, drop for raster
  }

  # --- 3. Extract Values and Calculate Distances ---
  if (is_raster) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("Package 'terra' is required for raster predictions.")
    }
    if (!all(env_vars %in% names(newdata))) {
      stop("Not all environmental variables from the ellipsoid are present in the SpatRaster.")
    }

    # Use as.data.frame to guarantee a flat ncell x nlyr matrix
    val_df <- terra::as.data.frame(newdata[[env_vars]], na.rm = FALSE)
    val_mat <- as.matrix(val_df)

  } else {
    if (!is.data.frame(newdata) && !is.matrix(newdata)) {
      stop("`newdata` must be a SpatRaster, data.frame, or matrix.")
    }
    if (!all(env_vars %in% colnames(newdata))) {
      stop("Not all environmental variables from the ellipsoid are present in the newdata.")
    }
    val_mat <- as.matrix(newdata[, env_vars, drop = FALSE])
  }

  # Initialize distances with NAs
  mah_dist <- rep(NA_real_, nrow(val_mat))
  valid_rows <- stats::complete.cases(val_mat)

  # Calculate Mahalanobis distance only on valid (non-NA) rows
  if (any(valid_rows)) {
    mah_dist[valid_rows] <- stats::mahalanobis(
      x = val_mat[valid_rows, , drop = FALSE],
      center = object$centroid,
      cov = object$covariance_matrix
    )
  }

  # --- 4. Compute Requested Outputs ---
  out_list <- list()

  if (include_mahalanobis) {
    out_list$mahalanobis <- mah_dist
  }

  if (mahalanobis_truncated) {
    mah_trunc <- mah_dist
    mah_trunc[mah_dist > chi_cutoff] <- NA_real_
    out_list$mahalanobis_trunc <- mah_trunc
  }

  if (include_suitability) {
    out_list$suitability <- exp(-0.5 * mah_dist)
  }

  if (suitability_truncated) {
    suit_trunc <- exp(-0.5 * mah_dist)
    suit_trunc[mah_dist > chi_cutoff] <- NA_real_
    out_list$suitability_trunc <- suit_trunc
  }

  if (length(out_list) == 0) {
    stop("At least one prediction output must be selected (e.g., include_suitability = TRUE).")
  }

  # --- 5. Format and Return Output ---
  if (is_raster) {
    # Reconstruct the raster layers
    res_rast <- terra::rast(newdata[[1]], nlyrs = length(out_list))
    names(res_rast) <- names(out_list)
    terra::values(res_rast) <- do.call(cbind, out_list)

    if (keep_data) {
      return(c(newdata, res_rast))
    } else {
      return(res_rast)
    }

  } else {
    # Reconstruct the data.frame
    res_df <- as.data.frame(out_list)

    if (keep_data) {
      return(cbind(newdata, res_df))
    } else {
      return(res_df)
    }
  }
}
