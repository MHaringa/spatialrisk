#' @noRd
concentration_hotspot_indexed <- function(
    data,
    value,
    top_n = 1,
    radius = 200,
    lon = "lon",
    lat = "lat",
    crs_metric = 3035,
    print_progress = TRUE,
    cell_size = radius
) {
  value <- validate_hotspot_value(value)
  validate_indexed_hotspot_input(data, value, top_n, radius, lon, lat,
                                 crs_metric, print_progress, cell_size)

  original <- data
  original$ix <- seq_len(nrow(original))

  metric <- convert_crs_df(original, crs_from = 4326, crs_to = crs_metric,
                           lon_from = lon, lat_from = lat,
                           lon_to = "x", lat_to = "y")

  remaining_metric <- metric
  pts_lst <- vector("list", top_n)
  conc_lst <- vector("list", top_n)
  output_col <- hotspot_sum_column(value)

  for (i in seq_len(top_n)) {
    best <- indexed_concentration_best_cpp(
      x_candidates = remaining_metric$x,
      y_candidates = remaining_metric$y,
      x_ref = remaining_metric$x,
      y_ref = remaining_metric$y,
      value_ref = remaining_metric[[value]],
      ix_ref = remaining_metric$ix,
      radius = radius,
      cell_width = cell_size
    )

    center_ll <- convert_crs_df(
      data.frame(x = best$x, y = best$y),
      crs_from = crs_metric,
      crs_to = 4326,
      lon_from = "x",
      lat_from = "y",
      lon_to = lon,
      lat_to = lat
    )

    selected <- indexed_points_in_radius_cpp(
      x_center = best$x,
      y_center = best$y,
      x_ref = remaining_metric$x,
      y_ref = remaining_metric$y,
      value_ref = remaining_metric[[value]],
      ix_ref = remaining_metric$ix,
      radius = radius,
      cell_width = cell_size
    )

    if (nrow(selected) == 0) {
      rlang::abort("No points found inside selected hotspot radius.",
                   call = NULL)
    }

    selected_rows <- original[match(selected$ix, original$ix), , drop = FALSE]
    selected_rows$distance_m <- selected$distance_m
    selected_rows$id <- i
    selected_rows[[output_col]] <- best$concentration[1]

    hc <- data.frame(
      lon = center_ll[[lon]][1],
      lat = center_ll[[lat]][1],
      value_sum = best$concentration[1],
      id = i
    )
    names(hc)[1:2] <- c(lon, lat)
    names(hc)[names(hc) == "value_sum"] <- output_col

    pts_lst[[i]] <- selected_rows
    conc_lst[[i]] <- hc

    if (top_n > 1 && print_progress) {
      cat("\rFinished", i, "of", top_n)
    }

    if (top_n > 1 && i < top_n) {
      remaining_metric <- remaining_metric[
        !remaining_metric$ix %in% selected$ix,
        ,
        drop = FALSE
      ]

      if (nrow(remaining_metric) == 0) {
        rlang::abort("Need more rows", call = NULL)
      }
    }
  }

  contributing_points <- do.call(rbind, pts_lst)
  hotspots <- do.call(rbind, conc_lst)
  output <- format_hotspot_output_columns(hotspots, contributing_points, value,
                                          lon, lat)
  hotspots <- output$hotspots
  contributing_points <- output$contributing_points
  rownames(contributing_points) <- NULL
  rownames(hotspots) <- NULL

  lst <- list(
    hotspots = hotspots,
    contributing_points = contributing_points
  )
  attr(lst, "radius") <- radius
  attr(lst, "rasterized") <- NULL
  attr(lst, "focal") <- NULL
  attr(lst, "threshold") <- NA_real_
  attr(lst, "value") <- value
  attr(lst, "lon") <- lon
  attr(lst, "lat") <- lat
  attr(lst, "crs_metric") <- crs_metric
  attr(lst, "method") <- "observed"
  class(lst) <- append("hotspot", class(lst))
  lst
}

validate_indexed_hotspot_input <- function(data, value, top_n, radius,
                                           lon, lat, crs_metric,
                                           print_progress, cell_size) {
  check_input(data, value, top_n, radius, radius, 1)

  column_args <- list(value, lon, lat)
  if (!all(vapply(column_args, is.character, logical(1))) ||
      !all(vapply(column_args, length, integer(1)) == 1) ||
      any(is.na(unlist(column_args, use.names = FALSE)))) {
    stop("Column arguments must be single non-missing strings.", call. = FALSE)
  }

  if (!all(c(lon, lat, value) %in% names(data))) {
    stop("`data` must contain columns '", lon, "', '", lat, "', and '",
         value, "'.", call. = FALSE)
  }

  if (!is.numeric(data[[lon]]) || !is.numeric(data[[lat]]) ||
      !is.numeric(data[[value]])) {
    stop("Columns '", lon, "', '", lat, "', and '", value,
         "' must be numeric.", call. = FALSE)
  }

  if (anyNA(data[[lon]]) || anyNA(data[[lat]])) {
    stop("Longitude and latitude columns must not contain missing values.",
         call. = FALSE)
  }

  if (anyNA(data[[value]])) {
    stop("`value` column must not contain missing values.", call. = FALSE)
  }

  output_col <- hotspot_sum_column(value)
  check_hotspot_output_name_conflicts(data, output_col)

  check_hotspot_crs_metric(crs_metric)

  if (!is.logical(print_progress) || length(print_progress) != 1 ||
      is.na(print_progress)) {
    stop("`print_progress` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(cell_size) || length(cell_size) != 1 ||
      is.na(cell_size) || !is.finite(cell_size) || cell_size <= 0) {
    stop("`cell_size` must be a single finite positive number.",
         call. = FALSE)
  }

  invisible(NULL)
}
