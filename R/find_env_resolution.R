#' Find an objective environmental grid resolution
#'
#' @description
#' Calculates an objective, data-driven grid resolution for environmental
#' thinning. For each environmental variable, the function selects a bandwidth
#' for a kernel-density estimate (KDE) of the marginal distribution. The chosen
#' bandwidth defines the spatial scale below which two observations carry
#' essentially redundant information, and is therefore a natural choice for the
#' edge length of an environmental grid cell.
#'
#' Three established bandwidth selectors are supported (see Details):
#' \itemize{
#'   \item \code{"sheather-jones"} (default) — the Sheather–Jones direct
#'         plug-in estimator (Sheather & Jones, 1991), the modern recommended
#'         default for non-Gaussian data;
#'   \item \code{"silverman"} — Silverman's rule of thumb (Silverman, 1986);
#'   \item \code{"scott"} — Scott's rule (Scott, 1992).
#' }
#'
#' @param data A \code{data.frame} containing the environmental variables.
#' @param env_vars A character vector specifying the environmental variables to
#'   analyse.
#' @param method The bandwidth selector. One of \code{"sheather-jones"}
#'   (default), \code{"silverman"}, or \code{"scott"}.
#'
#' @return An object of class \code{bean_env_resolution} (a list) with:
#'   \describe{
#'     \item{\code{suggested_resolution}}{A named numeric vector of the
#'           suggested grid resolution for each variable, in the units of that
#'           variable.}
#'     \item{\code{bandwidths}}{The bandwidths used to derive each resolution
#'           (identical to \code{suggested_resolution}).}
#'     \item{\code{density_data}}{A long-format \code{data.frame} of the kernel
#'           density estimates, used by \code{\link{plot.bean_env_resolution}}.}
#'     \item{\code{method}}{The bandwidth selector that was used.}
#'   }
#'
#' @details
#' \strong{Why a bandwidth?} A good environmental grid cell should be small
#' enough to distinguish ecologically meaningful differences, but large enough
#' to absorb sampling noise. A kernel density bandwidth chosen from the data
#' answers exactly that question: it is the scale at which the empirical
#' density of observations becomes smooth. Using it as the grid resolution
#' yields one occurrence per cell on average when the sampling intensity is
#' near the mode of the data.
#'
#' \strong{Selectors.}
#' \itemize{
#'   \item \emph{Sheather–Jones} (\code{stats::bw.SJ} with \code{method = "dpi"})
#'         is a plug-in selector that is robust for non-Gaussian densities and
#'         is the standard recommendation in the modern literature (Sheather &
#'         Jones, 1991; Jones, Marron & Sheather, 1996). Recommended default.
#'   \item \emph{Silverman} (\code{stats::bw.nrd0}) is the rule-of-thumb
#'         \eqn{h = 0.9 \, \min(\hat\sigma, IQR/1.34) \, n^{-1/5}} (Silverman,
#'         1986). Fast and stable, but assumes near-Gaussian shape.
#'   \item \emph{Scott} (\code{stats::bw.nrd}) is the Gaussian-optimal rule
#'         \eqn{h = 1.06 \, \hat\sigma \, n^{-1/5}} (Scott, 1992). Simpler than
#'         Silverman but less robust to outliers.
#' }
#'
#' If \code{"sheather-jones"} fails (this can happen with strongly tied data),
#' the function falls back to Silverman's rule for that variable and emits a
#' \code{message()}.
#'
#' @references
#' Sheather, S. J. & Jones, M. C. (1991). A reliable data-based bandwidth
#' selection method for kernel density estimation. \emph{Journal of the Royal
#' Statistical Society: Series B}, 53(3), 683–690.
#'
#' Silverman, B. W. (1986). \emph{Density Estimation for Statistics and Data
#' Analysis}. Chapman & Hall, London.
#'
#' Scott, D. W. (1992). \emph{Multivariate Density Estimation: Theory,
#' Practice, and Visualization}. Wiley, New York.
#'
#' Jones, M. C., Marron, J. S. & Sheather, S. J. (1996). A brief survey of
#' bandwidth selection for density estimation. \emph{Journal of the American
#' Statistical Association}, 91(433), 401–407.
#'
#' @seealso \code{\link{thin_env_nd}}, \code{\link{thin_env_center}},
#'   \code{\link[stats]{bw.SJ}}, \code{\link[stats]{bw.nrd0}}.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   bio1  = c(rnorm(200, 15, 2), rnorm(50, 25, 1)),
#'   bio12 = c(rnorm(200, 1200, 200), rnorm(50, 2500, 100))
#' )
#' res <- find_env_resolution(df, env_vars = c("bio1", "bio12"))
#' res$suggested_resolution
#' \donttest{plot(res)}
#'
#' @export
#' @importFrom stats bw.SJ bw.nrd0 bw.nrd density complete.cases sd
find_env_resolution <- function(data,
                                env_vars,
                                method = c("sheather-jones",
                                           "silverman",
                                           "scott")) {

  # --- Input validation ---------------------------------------------------
  method <- match.arg(method)
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!all(env_vars %in% names(data))) {
    missing_vars <- setdiff(env_vars, names(data))
    stop("Variables not found in `data`: ",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  clean_data <- data[stats::complete.cases(data[, env_vars, drop = FALSE]),
                     env_vars, drop = FALSE]

  if (nrow(clean_data) < 4) {
    stop("At least four complete observations are required.", call. = FALSE)
  }

  # --- Per-variable bandwidth estimation ----------------------------------
  bws <- vapply(env_vars, function(v) {
    x <- clean_data[[v]]
    .bean_bandwidth(x, method = method, var_name = v)
  }, numeric(1))
  names(bws) <- env_vars

  # --- Build density data for plotting ------------------------------------
  density_list <- lapply(env_vars, function(v) {
    x  <- clean_data[[v]]
    bw <- bws[[v]]
    d  <- stats::density(x, bw = bw)
    data.frame(variable = v,
               x        = d$x,
               density  = d$y,
               stringsAsFactors = FALSE)
  })
  density_data <- do.call(rbind, density_list)

  out <- list(
    suggested_resolution = bws,
    bandwidths           = bws,
    density_data         = density_data,
    method               = method
  )
  class(out) <- "bean_env_resolution"
  out
}

# Internal: compute a single bandwidth with a safe fallback.
.bean_bandwidth <- function(x, method, var_name) {
  if (length(unique(x)) < 2 || stats::sd(x) <= .Machine$double.eps) {
    stop(sprintf("Variable '%s' has no variability; cannot estimate a grid resolution.",
                 var_name), call. = FALSE)
  }

  bw <- switch(method,
    "sheather-jones" = tryCatch(stats::bw.SJ(x, method = "dpi"),
                                error = function(e) NA_real_),
    "silverman"      = stats::bw.nrd0(x),
    "scott"          = stats::bw.nrd(x)
  )

  if (is.na(bw) || bw <= 0) {
    if (method == "sheather-jones") {
      message(sprintf(
        "Sheather-Jones bandwidth failed for '%s'; falling back to Silverman's rule.",
        var_name))
      bw <- stats::bw.nrd0(x)
    } else {
      stop(sprintf("Bandwidth estimation failed for '%s'.", var_name),
           call. = FALSE)
    }
  }
  bw
}

#' Print method for \code{bean_env_resolution}
#'
#' @param x A \code{bean_env_resolution} object.
#' @param ... Unused.
#' @return Invisibly returns \code{x}.
#' @export
#' @keywords internal
print.bean_env_resolution <- function(x, ...) {
  cat("--- Bean environmental grid resolution ---\n")
  cat(sprintf("Bandwidth selector: %s\n\n", x$method))
  res_df <- data.frame(variable   = names(x$suggested_resolution),
                       resolution = unname(x$suggested_resolution))
  print(res_df, row.names = FALSE)
  invisible(x)
}

#' Plot method for \code{bean_env_resolution}
#'
#' Draws one panel per environmental variable showing the kernel density
#' estimate used to derive the suggested grid resolution. The bandwidth is
#' marked as a horizontal scale bar at the bottom of each panel.
#'
#' @param x A \code{bean_env_resolution} object.
#' @param ... Additional graphical parameters passed to \code{plot()}.
#' @return Invisibly returns \code{NULL}.
#' @export
#' @keywords internal
#' @importFrom graphics par plot abline points title axis box polygon legend
#' @importFrom grDevices adjustcolor
plot.bean_env_resolution <- function(x, ...) {
  vars   <- names(x$suggested_resolution)
  n_vars <- length(vars)
  cols   <- ceiling(sqrt(n_vars))
  rows   <- ceiling(n_vars / cols)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mfrow = c(rows, cols),
                oma   = c(1, 1, 4, 1),
                mar   = c(4, 4, 2, 1))

  for (v in vars) {
    dd  <- x$density_data[x$density_data$variable == v, ]
    bw  <- x$suggested_resolution[[v]]
    mid <- dd$x[which.max(dd$density)]

    graphics::plot(dd$x, dd$density, type = "l", lwd = 2, col = "grey20",
                   xlab = v, ylab = "Density",
                   main = sprintf("%s  (bw = %.4g)", v, bw), ...)
    graphics::polygon(c(dd$x, rev(dd$x)),
                      c(dd$density, rep(0, nrow(dd))),
                      col    = grDevices::adjustcolor("#118ab2", alpha.f = 0.25),
                      border = NA)
    # Scale bar showing the bandwidth (= suggested cell width).
    y_bar <- 0.02 * max(dd$density)
    graphics::segments(mid - bw / 2, y_bar, mid + bw / 2, y_bar,
                       col = "#ef476f", lwd = 3)
    graphics::points(c(mid - bw / 2, mid + bw / 2), c(y_bar, y_bar),
                     pch = 16, col = "#ef476f")
  }

  graphics::title(
    main = sprintf("Suggested grid resolution (%s)", x$method),
    sub  = "Red bar shows the bandwidth = suggested cell width",
    outer = TRUE,
    cex.main = 1.3,
    font.main = 2
  )
  invisible(NULL)
}
