#' Prepare data for environmental thinning
#'
#' @description
#' This function serves as a pre-processing step to clean and prepare species
#' occurrence data. It performs three key actions:
#' 1. Removes records with missing longitude or latitude values.
#' 2. Extracts environmental data from raster layers that are already scaled
#'    for each occurrence point.
#' 3. Removes records that fall outside the raster extent or have missing
#'    environmental data.
#' The final output is a clean data frame where the environmental variables
#' have a mean of 0 and a standard deviation of 1.
#'
#' @param data A data.frame of species occurrences records, including columns
#'   for longitude and latitude.
#' @param env_rasters A SpatRaster (from \code{terra} package) or RasterStack (from \code{raster} package)
#'   object of environmental variables.
#' @param longitude (character) The name of the longitude column in \code{data}.
#' @param latitude (character) The name of the latitude column in \code{data}.
#' @param transform (character) The transformation to apply to the environmental
#'   rasters before extracting data. Options are "scale" (default), "pca",
#'   or "none". See Details.
#'
#' @return A data.frame containing the cleaned and scaled occurrence data, with
#'   the following columns:
#'   \item{Original Columns}{All columns from the input \code{data} are preserved for the valid records.}
#'   \item{Environmental Variables}{New columns, named after the layers in \code{env_rasters}, containing the extracted and scaled environmental data.}
#' @details
#'
#' ### Environmental Variable Transformation
#'
#' The \code{transform} argument allows for different pre-processing of the
#' environmental raster layers to address issues like differing units and
#' multicollinearity.
#'
#' - "scale" (Default):** This is the standard approach to handle variables
#'   with different units (e.g., °C vs. mm). It transforms each raster layer to
#'   have a mean of 0 and a standard deviation of 1 (Baddeley et al., 2016). This process makes
#'   the variables equal variance. As a result,
#'   each variable contributes equally to the analysis, ensuring that the
#'   resulting resolutions are based on the relative distribution of data points
#'   within each environmental dimension, not their arbitrary original units
#'   (Beaugrand, 2024; Kléparski et al., 2021).
#'
#' - "pca": This option performs a Principal Component Analysis (PCA) on
#'   the environmental rasters. This is a powerful technique for dealing with
#'   multicollinearity (highly correlated variables). It transforms the original
#'   rasters into a new set of uncorrelated layers (Principal Components) (Qiao et al., 2016). The
#'   function then extracts the PC scores for each occurrence point.
#'
#' - "none": This option extracts the raw environmental values from the
#'   rasters without any transformation. This is suitable if your rasters are
#'   already scaled or if you have a specific reason to use the raw values.
#'
#' @references
#' Baddeley, A., Rubak, E. and Turner, R. (2016). Spatial point patterns: methodology and applications with R. CRC press.
#'
#' Beaugrand, G. (2024). An ecological niche model that considers local relationships among variables: The Environmental String Model. Ecosphere, 15(10), e70015.
#'
#' Kléparski, L., Beaugrand, G. and Edwards, M. (2021). Plankton biogeography in the North Atlantic Ocean and its adjacent seas: Species assemblages and environmental signatures. Ecology and Evolution, 11(10), 5135-5149.
#'
#' Qiao, H., Peterson, A. T., Campbell, L. P., Soberón, J., Ji, L. and Escobar, L. E. (2016). NicheA: creating virtual species and ecological niches in multivariate environmental scenarios. Ecography, 39(8), 805-813.
#'
#' @export
#' @importFrom terra extract rast
#' @importFrom stats complete.cases
#' @examples
#' \dontrun{
#' library(terra)
#'
#' # 1. Load sample data
#' bio1_file <- system.file("extdata", "BIO1.tif", package = "bean")
#' bio12_file <- system.file("extdata", "BIO12.tif", package = "bean")
#' env_rasters <- terra::rast(c(bio1_file, bio12_file))
#'
#' occ_file <- system.file("extdata", "Peromyscus_maniculatus_original.csv", package = "bean")
#' data <- read.csv(occ_file)
#'
#' # 2. Run the preparation function
#' prepared_data <- prepare_bean(
#'   data = data,
#'   env_rasters = env_rasters,
#'   longitude = "x",
#'   latitude = "y",
#'   scale = TRUE
#' )
#'
#' # 3. View the clean, scaled data
#' head(prepared_data)
#' summary(prepared_data)
#' }
prepare_bean <- function(data, env_rasters, longitude, latitude, transform = "scale") {
  # --- 1. Validate inputs and standardize raster format ---
  if (inherits(env_rasters, "RasterStack")) {
    env_rasters <- terra::rast(env_rasters)
  } else if (!inherits(env_rasters, "SpatRaster")) {
    stop("`env_rasters` must be a SpatRaster from 'terra' or a RasterStack from 'raster'.")
  }

  coord_vars <- c(longitude, latitude)
  if (!all(coord_vars %in% names(data))) {
    stop("Longitude and/or latitude columns not found in `data`.")
  }

  # --- 2. Remove initial NA coordinates ---
  n_start <- nrow(data)
  occ_clean_coords <- data[stats::complete.cases(data[, coord_vars]), ]
  n_after_coord_clean <- nrow(occ_clean_coords)

  if (n_start > n_after_coord_clean) {
    message(sprintf("%d records removed due to missing coordinates.", n_start - n_after_coord_clean))
  }

  # --- 3. Transform rasters based on the chosen method ---
  transformation_details <- list(method = transform)

  if (transform == "scale") {
    message("Scaling environmental rasters...")
    env_rasters_to_extract <- terra::scale(env_rasters)
    transformation_details$scale_center <- terra::global(env_rasters, "mean", na.rm = TRUE)
    transformation_details$scale_sd <- terra::global(env_rasters, "sd", na.rm = TRUE)

  } else if (transform == "pca") {
    message("Performing PCA on environmental rasters...")

    # Use terra's built-in prcomp function for a more direct approach.
    # It handles scaling internally and returns both the model and the PC rasters.
    # Note: This can be memory-intensive for very large rasters.
    pca_model <- terra::prcomp(env_rasters, scale = TRUE)

    # Project rasters onto PCs
    env_rasters_to_extract <- terra::predict(env_rasters, pca_model)
    names(env_rasters_to_extract) <- paste0("PC", 1:terra::nlyr(env_rasters_to_extract))

  } else if (transform == "none") {
    message("Skipping raster transformation.")
    env_rasters_to_extract <- env_rasters
  } else {
    stop("Invalid `transform` argument. Choose 'scale', 'pca', or 'none'.")
  }

  # --- 4. Extract environmental data ---
  message("Extracting environmental data for occurrence points...")
  env_data_extracted <- terra::extract(
    env_rasters_to_extract,
    occ_clean_coords[, coord_vars]
  )

  # Combine original coordinates with extracted environmental data
  occ_with_env <- cbind(occ_clean_coords, env_data_extracted)

  # --- 5. Remove points with NA environmental data ---
  occ_final <- occ_with_env[stats::complete.cases(occ_with_env), ]
  n_after_env_clean <- nrow(occ_final)

  if (n_after_coord_clean > n_after_env_clean) {
    message(sprintf("%d records removed because they fell outside the raster extent or had NA environmental values.",
                    n_after_coord_clean - n_after_env_clean))
  }

  message(sprintf("Data preparation complete. Returning %d clean records.", n_after_env_clean))

  # Remove the now-redundant "ID" column from extraction
  occ_final$ID <- NULL

  return(occ_final)
}

