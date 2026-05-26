#' Raw Rusa unicolor occurrence data
#'
#' Raw, unthinned occurrence records for Rusa unicolor (Sambar deer) in
#' Thailand. Used to demonstrate the spatial clustering and environmental
#' bias typical of SDM datasets.
#'
#' @format A \code{data.frame} with one row per occurrence and the columns:
#' \describe{
#'   \item{species}{Species name.}
#'   \item{x}{Longitude (decimal degrees).}
#'   \item{y}{Latitude (decimal degrees).}
#' }
#' @source Example dataset shipped with the \pkg{bean} package.
"occ_data_raw"

#' Cleaned and scaled occurrence data
#'
#' Output of \code{\link{prepare_bean}} applied to \code{occ_data_raw} using
#' the bundled environmental rasters. Missing coordinates and records outside
#' the raster extent have been removed, and environmental values have been
#' extracted and standardised.
#'
#' @format A \code{data.frame} with the columns:
#' \describe{
#'   \item{species}{Species name.}
#'   \item{x, y}{Coordinates.}
#'   \item{bio_1}{Scaled annual mean temperature.}
#'   \item{bio_4}{Scaled temperature seasonality.}
#'   \item{bio_12}{Scaled annual precipitation.}
#'   \item{bio_15}{Scaled precipitation seasonality.}
#' }
"origin_dat_prepared"

#' Stochastically thinned environmental data
#'
#' Result of \code{\link{thin_env_nd}} applied to
#' \code{\link{origin_dat_prepared}}. Contains one randomly chosen occurrence
#' per occupied environmental grid cell.
#'
#' @format A \code{bean_thinned} object (see \code{\link{thin_env_nd}}).
"thinned_stochastic"

#' Deterministically thinned environmental data
#'
#' Result of \code{\link{thin_env_center}} applied to
#' \code{\link{origin_dat_prepared}}. Contains one calculated centroid per
#' occupied environmental grid cell.
#'
#' @format A \code{bean_thinned_center} object (see
#'   \code{\link{thin_env_center}}).
"thinned_deterministic"

#' Fitted niche ellipsoid for Rusa unicolor
#'
#' A \code{bean_ellipsoid} fitted to \code{\link{origin_dat_prepared}}
#' representing the baseline environmental niche of the species.
#'
#' @format A \code{bean_ellipsoid} object (see \code{\link{fit_ellipsoid}}).
"origin_ellipse"
