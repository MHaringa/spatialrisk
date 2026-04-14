#' Find points within radius around one or more center coordinates
#'
#' @description This function selects rows from a data frame whose
#' longitude/latitude coordinates fall within a given radius (in meters)
#' from one or more specified center points. It also calculates the distance
#' of each point to the center.
#'
#' @param data A data frame containing at least longitude and latitude columns.
#' @param lon_center Numeric scalar or vector, longitude(s) of the circle center(s).
#' @param lat_center Numeric scalar or vector, latitude(s) of the circle center(s).
#' @param lon A string with the name of the longitude column in `data`.
#' @param lat A string with the name of the latitude column in `data`.
#' @param radius Numeric, circle radius in meters. Default is 200.
#' @param sort Logical, if `TRUE` results are sorted by distance within each center.
#'
#' @return A data frame subset of `data` with an extra column `distance_m` and
#'   if multiple centers are provided, also a column `center_index`.
#'
#' @export
points_within_radius <- function(data, lon_center, lat_center,
                                 lon = "lon", lat = "lat",
                                 radius = 200, sort = TRUE) {

  # Column checks
  if (!all(c(lon, lat) %in% names(data))) {
    stop("`data` does not contain columns ", lon, " and ", lat,
         call. = FALSE)
  }

  if (!is.numeric(data[[lon]]) || !is.numeric(data[[lat]])) {
    stop(lon, " and ", lat, " must be numeric.", call. = FALSE)
  }

  if (length(lon_center) != length(lat_center)) {
    stop("lon_center and lat_center must be the same length.", call. = FALSE)
  }

  df <- data.frame("lon" = data[[lon]], "lat" = data[[lat]])

  # Als meerdere centers → gebruik vectorized C++ functie
  if (length(lon_center) > 1) {
    incircle <- haversine_loop_cpp0(df, lat_center, lon_center, radius)
    data$id <- seq_len(nrow(data))
    incircle_df <- merge(x = incircle, y = data, by = "id", all.x = TRUE)
    if (sort) {
      incircle_df <- incircle_df[order(incircle_df$center_index,
                                       incircle_df$distance_m), ]
    }
    rownames(incircle_df) <- NULL
    incircle_df
  } else {
    # Enkele center → gebruik de snelle single-center C++ functie
    incircle <- haversine_loop_cpp(df, lat_center, lon_center, radius)
    incircle_df <- data[incircle$id, ]
    incircle_df$distance_m <- incircle$distance_m
    if (sort) {
      incircle_df <- incircle_df[order(incircle_df$distance_m), ]
    }
    incircle_df
  }
}

#' @rdname points_within_radius
#' @export
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `points_in_circle()` was renamed to [points_within_radius()].
points_in_circle <- function(data, lon_center, lat_center, lon = lon, lat = lat,
                             radius = 200, sort = TRUE) {
  lifecycle::deprecate_warn("0.7.5", "points_in_circle()", "points_within_radius()")

  # NSE capture
  lon <- deparse(substitute(lon))
  lat <- deparse(substitute(lat))

  points_within_radius(data = data,
                       lon_center = lon_center,
                       lat_center = lat_center,
                       lon = lon, lat = lat,
                       radius = radius, sort = sort)
}




