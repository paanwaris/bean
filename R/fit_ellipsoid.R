#' Fit an environmental niche ellipsoid
#'
#' @description Fits an ellipsoid that encompasses a chosen proportion of the
#' data points in an environmental space of two or more dimensions. The
#' centroid and covariance matrix can be estimated either by the classical
#' sample moments (\code{"covmat"}) or by the robust Minimum Volume Ellipsoid
#' (\code{"mve"}; Rousseeuw, 1985). Points are classified as inside or outside
#' the ellipsoid using a \eqn{\chi^2} cutoff on their squared Mahalanobis
#' distance.
#'
#' @param data A \code{data.frame} containing the environmental variables.
#' @param env_vars A character vector of at least two column names in
#'   \code{data} representing the environmental variables.
#' @param method One of \code{"covmat"} (default, classical) or \code{"mve"}
#'   (robust Minimum Volume Ellipsoid via \code{\link[MASS]{cov.mve}}).
#' @param level A single number in \code{(0, 1)}: the confidence level of the
#'   ellipsoid. Default \code{0.95}.
#'
#' @return An object of class \code{c("bean_ellipsoid", "nicheR_ellipsoid")}
#'   (a list) with:
#'   \describe{
#'     \item{\code{centroid}}{Named vector of variable means / centre.}
#'     \item{\code{covariance_matrix}, \code{cov_matrix}}{The covariance
#'           matrix used. Both names point to the same object; the former is
#'           kept for backward compatibility, the latter is the name expected
#'           by the \pkg{nicheR} package.}
#'     \item{\code{Sigma_inv}}{The inverse of \code{cov_matrix}, pre-computed
#'           so that \code{nicheR::predict()} does not have to invert it on
#'           every call.}
#'     \item{\code{dimensions}}{Integer, the number of environmental
#'           variables.}
#'     \item{\code{var_names}}{Character vector of the variable names used to
#'           fit the ellipsoid.}
#'     \item{\code{cl}}{Confidence level (same value as
#'           \code{parameters$level}); name expected by \pkg{nicheR}.}
#'     \item{\code{chi2_cutoff}}{The chi-square threshold,
#'           \code{stats::qchisq(level, df = dimensions)}.}
#'     \item{\code{niche_ellipse}}{A \code{data.frame} of polygon vertices for
#'           the 2-D ellipse. \code{NULL} when more than two variables are
#'           supplied (the 3-D mesh is generated lazily on plot).}
#'     \item{\code{all_points_used}}{Complete-case input data.}
#'     \item{\code{points_in_ellipse}}{Subset inside the ellipsoid.}
#'     \item{\code{points_outside_ellipse}}{Subset outside the ellipsoid.}
#'     \item{\code{inside_indices}}{Row indices (in \code{all_points_used})
#'           classified as inside.}
#'     \item{\code{parameters}}{List with \code{level} and \code{method}.}
#'   }
#'
#'   The object carries two S3 classes: \code{"bean_ellipsoid"} (used by
#'   \code{print()} and \code{plot()} in this package) and
#'   \code{"nicheR_ellipsoid"} (used by \code{nicheR::predict()} once that
#'   package is available on CRAN). Both methods work on the same object;
#'   the appropriate one is dispatched depending on which package is
#'   attached.
#'
#' @details
#' \strong{Methods.} \code{"covmat"} uses the sample mean and sample covariance
#' matrix. It is optimal under multivariate normality but sensitive to
#' outliers. \code{"mve"} (Rousseeuw, 1985) finds the smallest-volume ellipsoid
#' that contains a fraction of the data and is robust to a moderate proportion
#' of contaminating points.
#'
#' \strong{Confidence level.} Assuming approximate multivariate normality, the
#' boundary of the ellipsoid is the set of points whose squared Mahalanobis
#' distance equals \code{qchisq(level, df = n_dim)}.
#'
#' @references
#' If you intend to project a \code{bean_ellipsoid} into geographic space,
#' please install the \pkg{nicheR} package and use its \code{predict()}
#' method; the dual S3 class on the returned object allows
#' \code{nicheR::predict()} to dispatch on it directly. If you use the
#' prediction step in published work, please cite \pkg{nicheR}:
#'
#' Castaneda-Guzman, M., Hughes, C., Paansri, P. & Cobos, M. E. (2026).
#' \emph{nicheR: Ellipsoid-Based Virtual Niches and Visualization.}
#' R package version 0.1.0. \url{https://github.com/castanedaM/nicheR}.
#'
#' Rousseeuw, P. J. (1985). Multivariate estimation with high breakdown point.
#' In \emph{Mathematical Statistics and Applications, Vol. B}, 283–297.
#'
#' Van Aelst, S. & Rousseeuw, P. (2009). Minimum volume ellipsoid.
#' \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 1(1),
#' 71–82.
#'
#' Cobos, M. E., Osorio-Olvera, L., Soberón, J., Peterson, A. T., Barve, V. &
#' Barve, N. (2024). ellipsenm: ecological niches' characterizations using
#' ellipsoids. \url{https://github.com/marlonecobos/ellipsenm}.
#'
#' @examples
#' set.seed(81)
#' env_data <- data.frame(
#'   BIO1  = c(rnorm(50, 10, 1), 30),
#'   BIO12 = c(rnorm(50, 20, 2), 50)
#' )
#' fit <- fit_ellipsoid(env_data, env_vars = c("BIO1", "BIO12"),
#'                      method = "covmat", level = 0.95)
#' print(fit)
#' \donttest{plot(fit)}
#'
#' @export
#' @importFrom stats cov qchisq mahalanobis complete.cases
#' @importFrom MASS cov.mve
fit_ellipsoid <- function(data, env_vars, method = "covmat", level = 0.95) {

  # --- Input validation ---------------------------------------------------
  method <- match.arg(method, c("covmat", "mve"))
  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
    stop("`level` must be a single number in (0, 1).", call. = FALSE)
  }
  if (length(env_vars) < 2L) {
    stop("`env_vars` must contain at least two variable names.", call. = FALSE)
  }
  if (!all(env_vars %in% names(data))) {
    missing_vars <- setdiff(env_vars, names(data))
    stop("Variables not found in `data`: ",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  clean_data <- data[stats::complete.cases(data[, env_vars]), , drop = FALSE]
  n_dim <- length(env_vars)

  if (nrow(clean_data) < n_dim + 1L) {
    stop("At least n_variables + 1 complete observations are required.",
         call. = FALSE)
  }

  env_matrix <- as.matrix(clean_data[, env_vars])

  # --- Centroid, covariance, inclusion ------------------------------------
  if (method == "covmat") {
    centroid <- colMeans(env_matrix)
    cov_mat  <- stats::cov(env_matrix)
    mdist_sq <- stats::mahalanobis(env_matrix, center = centroid, cov = cov_mat)
    threshold <- stats::qchisq(level, df = n_dim)
    inside_indices <- which(mdist_sq <= threshold)
  } else {
    n_quant <- min(floor(nrow(env_matrix) * level), nrow(env_matrix))
    mve_res <- MASS::cov.mve(env_matrix, quantile.used = n_quant)
    centroid <- mve_res$center
    cov_mat  <- mve_res$cov
    inside_indices <- mve_res$best
  }

  # --- 2-D polygon (always cheap and rgl-free) ----------------------------
  ellipse_obj <- NULL
  if (n_dim == 2L) {
    ellipse_obj <- .bean_ellipse_polygon(centroid, cov_mat,
                                         level = level, n_points = 100L)
    colnames(ellipse_obj) <- env_vars
    ellipse_obj <- as.data.frame(ellipse_obj)
  }
  # For n_dim >= 3 we defer mesh creation to plot.bean_ellipsoid()
  # so that 'rgl' remains a soft dependency.

  outside_indices <- setdiff(seq_len(nrow(clean_data)), inside_indices)

  # Ensure dimnames so downstream functions (including nicheR::predict)
  # can match by variable name.
  if (is.null(names(centroid))) names(centroid) <- env_vars
  dimnames(cov_mat) <- list(env_vars, env_vars)

  # Pre-compute the inverse covariance matrix for nicheR::predict().
  # Fall back to a Moore-Penrose pseudo-inverse via SVD if the matrix is
  # numerically singular (e.g. perfectly collinear environmental variables).
  Sigma_inv <- tryCatch(
    solve(cov_mat),
    error = function(e) {
      sv <- svd(cov_mat)
      tol <- max(dim(cov_mat)) * .Machine$double.eps * max(sv$d)
      d_inv <- ifelse(sv$d > tol, 1 / sv$d, 0)
      pinv <- sv$v %*% diag(d_inv, length(d_inv)) %*% t(sv$u)
      dimnames(pinv) <- dimnames(cov_mat)
      warning("Covariance matrix is singular; using Moore-Penrose pseudo-inverse.",
              call. = FALSE)
      pinv
    }
  )

  results <- list(
    # ---- Fields used by bean's own methods --------------------------------
    centroid               = centroid,
    covariance_matrix      = cov_mat,
    niche_ellipse          = ellipse_obj,
    all_points_used        = as.data.frame(clean_data),
    points_in_ellipse      = as.data.frame(clean_data[inside_indices, ,
                                                      drop = FALSE]),
    points_outside_ellipse = as.data.frame(clean_data[outside_indices, ,
                                                      drop = FALSE]),
    inside_indices         = inside_indices,
    parameters             = list(level = level, method = method),
    # ---- Fields expected by nicheR::predict.nicheR_ellipsoid -------------
    dimensions             = n_dim,
    cov_matrix             = cov_mat,
    Sigma_inv              = Sigma_inv,
    cl                     = level,
    chi2_cutoff            = stats::qchisq(level, df = n_dim),
    var_names              = env_vars
  )

  # Dual class: bean methods dispatch on "bean_ellipsoid" (first), nicheR's
  # predict() dispatches on "nicheR_ellipsoid" (second). nicheR does not have
  # to be installed for the object to be created or for bean's own methods to
  # work; it is only consulted by R when the user calls predict() with nicheR
  # loaded.
  class(results) <- c("bean_ellipsoid", "nicheR_ellipsoid")
  results
}

# Internal: compute the perimeter polygon of a 2-D confidence ellipse.
.bean_ellipse_polygon <- function(centroid, cov_mat, level, n_points = 100L) {
  radius <- sqrt(stats::qchisq(level, df = 2L))
  e <- eigen(cov_mat[1:2, 1:2])
  transform_mat <- e$vectors %*% diag(sqrt(pmax(e$values, 0)))
  angles <- seq(0, 2 * pi, length.out = n_points)
  unit_circle <- cbind(cos(angles), sin(angles))
  t(centroid[1:2] + radius * transform_mat %*% t(unit_circle))
}

#' @export
#' @keywords internal
print.bean_ellipsoid <- function(x, ...) {
  cat("-- Bean Environmental Niche Ellipsoid --\n")
  cat(sprintf("Method      : %s\n", x$parameters$method))
  cat(sprintf("Dimensions  : %d (%s)\n",
              length(x$centroid),
              paste(names(x$centroid), collapse = ", ")))
  cat(sprintf("Level       : %.2f%%\n", x$parameters$level * 100))
  cat(sprintf("Points used : %d  (inside: %d, %.1f%%)\n",
              nrow(x$all_points_used),
              nrow(x$points_in_ellipse),
              100 * nrow(x$points_in_ellipse) /
                max(1L, nrow(x$all_points_used))))
  cat("Centroid:\n")
  print(x$centroid)
  invisible(x)
}

#' Plot a fitted \code{bean_ellipsoid}
#'
#' Draws a 2-D ellipse using base R graphics, or a 3-D interactive ellipsoid
#' using \pkg{rgl} (a suggested dependency). If \pkg{rgl} is not installed and
#' a 3-D plot is requested, the function falls back to the 2-D view of the
#' first two dimensions and emits a \code{message()}.
#'
#' @param x A \code{bean_ellipsoid} object.
#' @param dims Either a numeric vector of column indices or a character vector
#'   of variable names. Length 2 draws a 2-D plot, length >=3 draws a 3-D plot
#'   using the first three.
#' @param window_size Numeric length-2 size of the rgl window. Default
#'   \code{c(800, 800)}.
#' @param ... Unused.
#' @return Invisibly \code{NULL}.
#' @export
#' @keywords internal
#' @importFrom grDevices adjustcolor
#' @importFrom graphics plot points polygon legend
plot.bean_ellipsoid <- function(x, dims = c(1, 2), ...,
                                window_size = c(800, 800)) {
  env_vars  <- names(x$centroid)
  total_dim <- length(env_vars)

  if (is.numeric(dims)) {
    if (any(dims < 1 | dims > total_dim)) {
      stop("Numeric `dims` indices are out of bounds.", call. = FALSE)
    }
    plot_vars <- env_vars[dims]
  } else if (is.character(dims)) {
    if (!all(dims %in% env_vars)) {
      stop("One or more `dims` not found in the ellipsoid variables.",
           call. = FALSE)
    }
    plot_vars <- dims
  } else {
    stop("`dims` must be numeric or character.", call. = FALSE)
  }

  n_plot_dim <- length(plot_vars)
  level <- x$parameters$level

  colors <- list(
    inside       = "#118ab2",
    outside      = "#ef476f",
    ellipse_fill = "#06d6a0",
    ellipse_line = "#073b4c",
    centroid     = "#ffd166"
  )

  plot_data_all <- x$all_points_used
  point_status  <- rep("Outside", nrow(plot_data_all))
  point_status[x$inside_indices] <- "Inside"
  point_colors <- ifelse(point_status == "Inside",
                         colors$inside, colors$outside)

  # ------------- 2-D plot (base R) ---------------------------------------
  if (n_plot_dim == 2L) {
    var1 <- plot_vars[1]; var2 <- plot_vars[2]
    centroid_2d <- x$centroid[plot_vars]
    cov_2d      <- x$covariance_matrix[plot_vars, plot_vars]
    ellipse_pts <- .bean_ellipse_polygon(centroid_2d, cov_2d, level)

    x_range <- range(c(plot_data_all[[var1]], ellipse_pts[, 1]), na.rm = TRUE)
    y_range <- range(c(plot_data_all[[var2]], ellipse_pts[, 2]), na.rm = TRUE)

    graphics::plot(x_range, y_range, type = "n",
         xlab = var1, ylab = var2,
         main = "Fitted Environmental Niche Ellipse",
         sub  = sprintf("Method: %s | Level: %g%%",
                        x$parameters$method, level * 100))

    graphics::polygon(ellipse_pts[, 1], ellipse_pts[, 2],
            col    = grDevices::adjustcolor(colors$ellipse_fill, alpha.f = 0.3),
            border = colors$ellipse_line, lwd = 2)

    graphics::points(plot_data_all[[var1]], plot_data_all[[var2]],
           col = grDevices::adjustcolor(point_colors, alpha.f = 0.8),
           pch = 16, cex = 1.2)

    graphics::points(centroid_2d[1], centroid_2d[2],
           col = colors$ellipse_line, bg = colors$centroid,
           pch = 22, cex = 1.8, lwd = 2)

    graphics::legend("topright",
           legend = c("Inside", "Outside", "Centroid"),
           col    = c(colors$inside, colors$outside, colors$ellipse_line),
           pt.bg  = c(NA, NA, colors$centroid),
           pch    = c(16, 16, 23),
           bty    = "n", pt.cex = 1.2, inset = 0.02)

    return(invisible(NULL))
  }

  # ------------- 3-D plot (rgl, optional) --------------------------------
  if (!requireNamespace("rgl", quietly = TRUE)) {
    message("Package 'rgl' is not installed; falling back to a 2-D plot of ",
            "the first two requested dimensions.")
    return(plot.bean_ellipsoid(x, dims = plot_vars[1:2], ...))
  }

  vars_3d <- plot_vars[1:3]
  if (n_plot_dim > 3L) {
    message(sprintf("Showing first three selected variables: %s, %s, %s.",
                    vars_3d[1], vars_3d[2], vars_3d[3]))
  }

  centroid_3d <- x$centroid[vars_3d]
  cov_3d      <- x$covariance_matrix[vars_3d, vars_3d]
  radius_3d   <- sqrt(stats::qchisq(level, df = 3L))
  mesh_3d     <- rgl::ellipse3d(cov_3d, centre = centroid_3d, t = radius_3d)

  points_3d <- plot_data_all[, vars_3d]

  rgl::open3d(windowRect = c(50, 50,
                             50 + window_size[1], 50 + window_size[2]))
  rgl::bg3d("#f7f7f7")
  base_radius <- 0.01 * diff(range(unlist(points_3d), na.rm = TRUE))

  rgl::plot3d(
    x = points_3d[[1]], y = points_3d[[2]], z = points_3d[[3]],
    col = point_colors, type = "s", radius = base_radius,
    xlab = vars_3d[1], ylab = vars_3d[2], zlab = vars_3d[3],
    aspect = "iso", lit = TRUE
  )
  rgl::spheres3d(centroid_3d[1], centroid_3d[2], centroid_3d[3],
                 col = colors$centroid, radius = base_radius * 2.5, lit = TRUE)
  rgl::wire3d(mesh_3d, col = colors$ellipse_line, alpha = 0.4, lit = FALSE)

  invisible(NULL)
}
