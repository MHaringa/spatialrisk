#' Sum values within a radius around target coordinates
#'
#' @description Calculates the sum of all observations from a reference data set
#' that fall within a given radius (in meters) of each target point.
#'
#' @param targets A data.frame of target points for which sums are calculated.
#'   Must include at least columns for longitude and latitude.
#' @param reference A data.frame containing reference points. Must include at least
#'   columns for longitude, latitude, and the value of interest to summarize.
#' @param value A string giving the name of the column in `reference` to be summed.
#' @param lon_targets A string with the name of the longitude column in `targets`.
#'   Default is `"lon"`.
#' @param lat_targets A string with the name of the latitude column in `targets`.
#'   Default is `"lat"`.
#' @param lon_reference A string with the name of the longitude column in `reference`.
#'   Default is `"lon"`.
#' @param lat_reference A string with the name of the latitude column in `reference`.
#'   Default is `"lat"`.
#' @param radius Numeric. Radius of the circle in meters. Must be positive
#'   (default: 200).
#' @param progress Logical. Whether to display a progress bar. Default is `TRUE`.
#' @param result_col A string giving the name of the output column.
#'   Default is `"radius_sum"`.
#'
#' @return A data.frame equal to `targets` with an additional numeric column
#'   named by `result_col` containing the summed values from `reference`.
#'
#' @details
#' This function uses a C++ backend for efficient distance calculations
#' (Haversine formula).
#'
#' @useDynLib spatialrisk
#' @importFrom Rcpp sourceCpp
#' @importFrom Rcpp evalCpp
#' @import RcppProgress
#' @importFrom rlang abort
#'
#' @author Martin Haringa
#'
#' @examples
#' targets <- data.frame(location = c("p1", "p2"),
#'                       lon = c(6.561561, 6.561398),
#'                       lat = c(53.21369, 53.21326))
#'
#' reference <- data.frame(lon = c(6.5614, 6.5620, 6.5630),
#'                         lat = c(53.2132, 53.2140, 53.2150),
#'                         amount = c(10, 20, 15))
#'
#' radius_sum(targets, reference, value = "amount", radius = 100,
#'            progress = FALSE)
#'
#' @export
radius_sum <- function(targets, reference, value,
                       lon_targets = "lon", lat_targets = "lat",
                       lon_reference = "lon", lat_reference = "lat",
                       radius = 200, progress = TRUE,
                       result_col = "radius_sum") {

  if (!is.numeric(radius) || length(radius) != 1 || !is.finite(radius) ||
      radius <= 0) {
    rlang::abort(
      c(
        paste0("Can't calculate sums with `radius = ", radius, "`."),
        "x" = "`radius` must be a single finite positive number."
      ),
      call = NULL
    )
  }

  if (!is.logical(progress) || length(progress) != 1 ||
      is.na(progress)) {
    stop("`progress` must be TRUE or FALSE.", call. = FALSE)
  }

  column_args <- list(value, lon_targets, lat_targets, lon_reference,
                      lat_reference, result_col)
  if (!all(vapply(column_args, is.character, logical(1))) ||
      !all(vapply(column_args, length, integer(1)) == 1) ||
      any(is.na(unlist(column_args, use.names = FALSE)))) {
    stop("Column arguments must be single non-missing strings.", call. = FALSE)
  }

  if (!all(c(lon_targets, lat_targets) %in% names(targets))) {
    stop("`targets` does not contain columns '", lon_targets, "' and '",
         lat_targets, "'.", call. = FALSE)
  }

  if (!all(c(lon_reference, lat_reference, value) %in% names(reference))) {
    stop("`reference` does not contain required columns '", lon_reference,
         "', '", lat_reference, "', and '", value, "'.", call. = FALSE)
  }

  if (result_col %in% names(targets)) {
    stop("`targets` already contains column '", result_col, "'. Choose a ",
         "different `result_col` to avoid overwriting data.", call. = FALSE)
  }

  numeric_cols <- list(
    lon_targets = targets[[lon_targets]],
    lat_targets = targets[[lat_targets]],
    lon_reference = reference[[lon_reference]],
    lat_reference = reference[[lat_reference]],
    value = reference[[value]]
  )
  invalid_numeric <- names(numeric_cols)[
    !vapply(numeric_cols, is.numeric, logical(1))
  ]

  if (length(invalid_numeric) > 0) {
    stop("The following columns must be numeric: ",
         paste(invalid_numeric, collapse = ", "),
         call. = FALSE)
  }

  targets_df <- data.frame("lon" = targets[[lon_targets]],
                           "lat" = targets[[lat_targets]])
  reference_df <- data.frame("lon" = reference[[lon_reference]],
                             "lat" = reference[[lat_reference]],
                             "value" = reference[[value]])

  res <- concentration_loop_cpp(targets_df, reference_df, radius,
                                progress)
  targets[[result_col]] <- res$cumulation
  targets
}
