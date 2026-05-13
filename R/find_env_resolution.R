#' Find objective environmental resolution using the Nearest Neighbor Elbow Method
#'
#' @description Calculates an objective, data-driven grid resolution for environmental
#' thinning. Instead of relying on user-defined quantiles, this function analyzes the
#' 1D Nearest Neighbor (NN) distances for each environmental variable. It uses the
#' geometric "elbow" method (point of maximum curvature) to identify the exact distance
#' where dense artificial clustering transitions into natural data spacing.
#'
#' @param data A data.frame containing environmental variables.
#' @param env_vars A character vector specifying the environmental variables to analyze.
#'
#' @return An object of class \code{bean_env_resolution}
#' @export
find_env_resolution <- function(data, env_vars) {

  # --- Input Validation ---
  if (!all(env_vars %in% names(data))) {
    stop("One or more `env_vars` not found in the data frame")
  }

  clean_data <- data[stats::complete.cases(data[, env_vars]), env_vars, drop = FALSE]

  if (nrow(clean_data) < 4) {
    stop("At least 4 complete observations are needed to calculate an elbow curve.")
  }

  cat("Calculating nearest neighbor environmental distances and detecting elbows...\n")

  resolution_list <- lapply(env_vars, function(var) {
    val <- clean_data[[var]]
    n <- length(val)

    # 1. Fast 1D Nearest Neighbor Calculation (O(n log n))
    sorted_val <- sort(val)
    nn_distances <- numeric(n)

    # Edges only have one neighbor
    nn_distances[1] <- sorted_val[2] - sorted_val[1]
    nn_distances[n] <- sorted_val[n] - sorted_val[n-1]

    # Middle points look left and right for the closest neighbor
    for(i in 2:(n-1)) {
      nn_distances[i] <- min(sorted_val[i] - sorted_val[i-1],
                             sorted_val[i+1] - sorted_val[i])
    }

    # 2. Sort the NN distances to form the growth curve
    sorted_nn <- sort(nn_distances)

    # 3. Geometric Elbow Detection (Kneedle Algorithm Logic)
    x <- 1:n
    y <- sorted_nn

    # Define the line connecting the first and last points of the curve: Ax + By + C = 0
    A <- y[n] - y[1]
    B <- x[1] - x[n]
    C <- (x[n] * y[1]) - (x[1] * y[n])

    # Calculate perpendicular distance from every point on the curve to the line
    dist_to_line <- abs(A * x + B * y + C) / sqrt(A^2 + B^2)

    # The elbow is the point furthest from the straight line
    elbow_index <- which.max(dist_to_line)
    suggested_res <- y[elbow_index]

    # --- ZERO HANDLING FIX ---
    # If the elbow falls on the 'last zero' before the curve bends,
    # shift the index forward to the first ecologically meaningful (non-zero) distance.
    if (suggested_res <= 1e-8) {
      first_non_zero <- which(y > 1e-8)[1]

      if (!is.na(first_non_zero)) {
        elbow_index <- first_non_zero
        suggested_res <- y[elbow_index]
      } else {
        # Fallback if literally every single point has the exact same environmental value
        suggested_res <- 1e-4
        warning(sprintf("Variable %s has zero variance. A fallback resolution of 1e-4 was applied.", var))
      }
    }

    return(list(
      resolution = suggested_res,
      distances = y,
      elbow_x = elbow_index
    ))
  })

  # --- Construct Output ---
  names(resolution_list) <- env_vars
  suggested_res <- sapply(resolution_list, `[[`, "resolution")

  # Prepare plotting data
  distance_df_list <- lapply(seq_along(env_vars), function(i) {
    data.frame(
      variable = env_vars[i],
      point_index = 1:length(resolution_list[[i]]$distances),
      nn_distance = resolution_list[[i]]$distances,
      is_elbow = (1:length(resolution_list[[i]]$distances)) == resolution_list[[i]]$elbow_x
    )
  })

  curve_data <- do.call(rbind, distance_df_list)

  results <- list(
    suggested_resolution = suggested_res,
    curve_data = curve_data
  )

  class(results) <- "bean_env_resolution"
  return(results)
}

#' @export
#' @keywords internal
#' @importFrom graphics par plot abline points title
plot.bean_env_resolution <- function(x, ...) {

  # Extract variables and resolutions
  vars <- names(x$suggested_resolution)
  n_vars <- length(vars)

  # --- Dynamically Calculate Grid Layout (like facet_wrap) ---
  cols <- ceiling(sqrt(n_vars))
  rows <- ceiling(n_vars / cols)

  # Save original graphics parameters to restore later
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  # Setup the multi-plot grid
  # oma adds an outer margin for the main title, mar sets the margins for each individual plot
  graphics::par(mfrow = c(rows, cols),
                oma = c(1, 1, 4, 1),
                mar = c(4, 4, 2, 1))

  # --- Loop through each variable to draw the curves ---
  for (var in vars) {
    # Subset data for this specific variable
    var_data <- x$curve_data[x$curve_data$variable == var, ]
    res_val <- x$suggested_resolution[var]
    elbow_pt <- var_data[var_data$is_elbow, ]

    # 1. Draw the base line graph
    graphics::plot(var_data$point_index, var_data$nn_distance,
                   type = "l", col = "grey30", lwd = 2,
                   xlab = "Sorted Points", ylab = "NN Distance",
                   main = var, font.main = 2, cex.main = 1.2)

    # 2. Add the blue dashed threshold line
    graphics::abline(h = res_val, col = "blue", lty = 2, lwd = 2)

    # 3. Add the red dot marking the exact elbow point
    if (nrow(elbow_pt) > 0) {
      graphics::points(elbow_pt$point_index, elbow_pt$nn_distance,
                       col = "red", pch = 16, cex = 1.8)
    }
  }

  # --- Add the Master Title and Subtitle ---
  graphics::title(
    main = "k-Nearest Neighbor Distance Curves (Environmental Space)",
    sub = "Red dot and blue dashed line indicate the objective 'Elbow' resolution",
    outer = TRUE,
    cex.main = 1.5,
    font.main = 2,
    col.sub = "grey20"
  )

  return(invisible(NULL))
}
