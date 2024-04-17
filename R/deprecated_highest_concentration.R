#' Highest concentration risk
#'
#' @description Find the centre coordinates of a circle with a fixed radius that
#' maximizes the coverage of total fire risk insured. `highest_concentration()`
#' returns the coordinates (lon/lat) and the corresponding concentration. The
#' concentration is defined as the sum of all observations within a circle of a
#' certain radius. See \code{\link{concentration}} for determining concentration
#' for pre-defined coordinates.
#'
#' @param df data.frame of locations, should at least include column for
#' longitude, latitude and sum insured.
#' @param value column name with value of interest to summarize (e.g. sum
#' insured).
#' @param lon column name with longitude (defaults to `lon`).
#' @param lat column name with latitude (defaults to `lat`).
#' @param radius radius (in meters) (default is 200m).
#' @param grid_distance distance (in meters) for precision of concentration risk
#' (default is 25m). `neighborhood_search()` can be used to search for
#' coordinates with even higher concentrations in the neighborhood of the
#' highest concentrations.
#' @param display_progress show progress bar (TRUE/FALSE). Defaults to TRUE.
#' @param lowerbound set lowerbound.
#' @param gh_precision set precision for geohash.
#'
#' @details A recently European Commission regulation requires insurance
#' companies to determine the maximum value of insured fire risk policies of
#' all buildings that are partly or fully located within circle of a radius of
#' 200m (Commission Delegated Regulation (EU), 2015, Article 132). The problem
#' can be stated as: "find the centre coordinates of a circle with a fixed
#' radius that maximizes the coverage of total fire risk insured". This can be
#' viewed as a particular instance of the Maximal Covering Location Problem
#' (MCLP) with fixed radius. See Gomes (2018) for a solution to the maximum fire
#' risk insured capital problem using a multi-start local search meta-heuristic.
#' The computational performance of \code{highest_concentration()} is
#' investigated to overcome the long times the MCLP algorithm is taking.
#' \code{highest_concentration()} is written in C++, and for 500,000 buildings
#' it needs about 5-10 seconds to determine the maximum value of insured fire
#' risk policies that are partly or fully located within circle of a radius of
#' 200m.
#'
#' @import data.table
#' @importFrom lifecycle deprecate_warn
#'
#' @author Martin Haringa
#'
#' @return data.frame with coordinates (lon/lat) with the highest concentrations
#'
#' @references Commission Delegated Regulation (EU) (2015). Solvency II
#' Delegated Act 2015/35. Official Journal of the European Union, 58:124.
#' @references Gomes M.I., Afonso L.B., Chibeles-Martins N., Fradinho J.M.
#' (2018). Multi-start Local Search Procedure for the Maximum Fire Risk Insured
#' Capital Problem. In: Lee J., Rinaldi G., Mahjoub A. (eds) Combinatorial
#' Optimization. ISCO 2018. Lecture Notes in Computer Science, vol 10856.
#' Springer, Cham. <doi:10.1007/978-3-319-96151-4_19>
#'
#' @examples
#'  \dontrun{
#' # Find highest concentration with a precision of a grid of 25 meters
#' hc1 <- highest_concentration(Groningen, amount, radius = 200,
#'  grid_distance = 25)
#'
#' # Look for coordinates with even higher concentrations in the
#' # neighborhood of the coordinates with the highest concentration
#' hc1_nghb <- neighborhood_gh_search(hc1, max.call = 7000)
#' print(hc1_nghb)
#'
#' # Create map with geohashes above the lowerbound
#' # The highest concentration lies in one of the geohashes
#' plot(hc1)
#'
#' # Create map with highest concentration
#' plot(hc1_nghb)
#' }
#'
#' @export
highest_concentration <- function(df, value, lon = lon, lat = lat,
                                  lowerbound = NULL, radius = 200,
                                  grid_distance = 25, gh_precision = 6,
                                  display_progress = TRUE) {

  lifecycle::deprecate_warn(
    when = "0.7.2",
    what = "highest_concentration()",
    details = "Please use `find_highest_concentration()` instead."
  )

  if (!requireNamespace("geohashTools", quietly = TRUE)) {
    stop("geohashTools is needed for this function to work. Install it via
         install.packages(\"geohashTools\")", call. = FALSE)
  }

  df <- as.data.frame(df)

  value_nm <- deparse(substitute(value))
  lon_nm <- deparse(substitute(lon))
  lat_nm <- deparse(substitute(lat))
  df_nm <- deparse(substitute(df))

  if (!all(c(value_nm) %in% names(df))) {
    stop(df_nm, " does not contain column specified in `value`.
         Specify with argument `value`.", call. = FALSE)
  }

  if (!all(c(lon_nm, lat_nm) %in% names(df))) {
    stop(df_nm, " does not contain columns ", lon_nm, " and ", lat_nm,
         ". Specify with arguments `lon` and `lat`.", call. = FALSE)
  }

  nrows1 <- nrow(df)

  # Remove rows with NA values
  df <- df[!is.na(df[[value_nm]]), ]

  if (nrows1 !=  nrow(df)) {
    warning(nrows1 - nrow(df), " NAs detected in ", value_nm, ".
            Rows with NAs removed.", call. = FALSE)
  }

  # Add geohash (length 5 is 4.89km x 4.89km; length 6 is 1.22 x 0.61km;
  # length 7 is 153m x 153m; length 8 is 38m x 19m)
  df[["geohash"]] <- geohashTools::gh_encode(latitude = df[[lat_nm]],
                                             longitude = df[[lon_nm]],
                                             precision = gh_precision)

  # Determine lower bound for concentration
  # (based on circles around 1000 highest sums insured)
  # This only works with some relatively high sums insured
  if (is.null(lowerbound)) {
    lowerbound_1 <- lower_bound_fn(df, value_nm,
                                   lat_nm = lat_nm,
                                   lon_nm = lon_nm,
                                   radius = radius,
                                   highest = 1000)
  }

  portfolio_dt <- data.table::data.table(df)

  # Calculate total sum insured per geohash
  gh_sum <- portfolio_dt[, .(gh_self_sum = sum(get(value_nm))), by = geohash]

  # Determine 8 neighbors of geohash and
  # calculate total sum insured for the 9 geohashes
  gh_sum_nghbrs <- add_gh_nghbrs_sum(gh_sum, "gh_self_sum")

  # Find lowerbound for use cases where sum insured is same for all coords
  lowerbound_2 <- lower_bound_fn2(gh_sum_nghbrs,
                                  full = portfolio_dt,
                                  col = value_nm,
                                  lat_nm = lat_nm,
                                  lon_nm = lon_nm,
                                  radius = radius)

  # Take max of both lowerbounds
  if (is.null(lowerbound)) {
    lowerbound <- max(lowerbound_1, lowerbound_2)
  }

  # Remove geohashes with a total sum insured lower than the lower bound
  gh_remaining <- gh_sum_nghbrs[gh_nghbrs_sum >= lowerbound]

  # Determine neighbors for remaining geohashes
  gh_remaining_nbs_lst <- geohashTools::gh_neighbors(gh_remaining$geohash,
                                                     self = TRUE)
  gh_remaining_nbs <- as.vector(unlist(gh_remaining_nbs_lst))

  # Coordinates or remaining points in portfolio within remaining geohashes
  pts_remaining <- portfolio_dt[geohash %in% gh_remaining_nbs]
  names(pts_remaining)[names(pts_remaining) == value_nm] <- "_sum_insured"

  # Determine centre point for each remaining geohash
  # (delta is distance to bound)
  gh_center_lst <- geohashTools::gh_decode(gh_remaining$geohash,
                                           include_delta = TRUE)
  gh_center <- data.table::as.data.table(gh_center_lst)

  # Determine sum of remaining points within bounding box of radius
  # around remaining geohashes
  bbox_sum <- add_gh_bbox_sum(gh_center,
                              pts_remaining,
                              lon_nm = lon_nm,
                              lat_nm = lat_nm,
                              radius = radius)
  gh_sum_bbox <- cbind(gh_remaining, bbox_sum)

  # Remove (again) geohashes with a total sum insured lower than the lower bound
  gh_remaining_bbox <- gh_sum_bbox[sum_bbox >= lowerbound]

  # Determine neighbors for remaining geohashes
  gh_remaining_bbox_lst <- geohashTools::gh_neighbors(gh_remaining_bbox$geohash,
                                                      self = TRUE)
  gh_remaining_bbox_vec <- as.vector(unlist(gh_remaining_bbox_lst))

  # Coordinates or remaining points in portfolio within remaining geohashes
  pts_remaining2 <- portfolio_dt[geohash %in% gh_remaining_bbox_vec]

  # Create grid points in remaining geohashes
  gh_grid <- create_grid_points(gh_remaining_bbox, meters = grid_distance)

  # Concentration for each grid point
  names(pts_remaining)[names(pts_remaining) == lat_nm] <- "lat"
  names(pts_remaining)[names(pts_remaining) == lon_nm] <- "lon"
  names(pts_remaining2)[names(pts_remaining2) == lat_nm] <- "lat"
  names(pts_remaining2)[names(pts_remaining2) == lon_nm] <- "lon"

  gh_grid_conc <- concentration(gh_grid,
                                pts_remaining,
                                value = `_sum_insured`,
                                radius = radius,
                                display_progress = display_progress)

  gh_grid_conc_sort <- gh_grid_conc[order(-concentration)]
  data.table::setcolorder(gh_grid_conc_sort,
                          c("concentration", "lon", "lat", "geohash"))

  attr(gh_grid_conc_sort, "value_nm") <- value_nm
  attr(gh_grid_conc_sort, "pts_remaining") <- pts_remaining2
  attr(gh_grid_conc_sort, "gh_remaining") <- gh_remaining_bbox
  attr(gh_grid_conc_sort, "radius") <- radius

  class(gh_grid_conc_sort) <- append("conc", class(gh_grid_conc_sort))
  return(gh_grid_conc_sort)
}


#' Search for coordinates with higher concentrations within geohash
#'
#' \code{\link{highest_concentration}} returns the highest concentration within
#' a portfolio based on grid points. However, higher concentrations can be
#' found within two grid points. `neighborhood_gh_search()` looks for even
#' higher concentrations in the neighborhood of the grid points with the highest
#' concentrations. This optimization is done by means of Simulated Annealing.
#'
#' @param hc object of class `concentration` obtained from
#' `highest_concentration()`
#' @param highest_geohash the number of geohashes the searching algorithm is
#' applied to. Defaults to 1 (i.e. algorithm is only applied to the geohash
#' with the highest concentration).
#' @param max.call maximum number of calls to the concentration function (i.e.
#' the number of coordinates in the neighborhood of the highest concentration).
#' Defaults to 1000.
#' @param verbose show messages from the algorithm (TRUE/FALSE). Defaults to
#' FALSE.
#' @param seed set seed
#'
#' @importFrom lifecycle deprecate_warn
#'
#' @author Martin Haringa
#'
#' @return data.frame
#'
#' @export neighborhood_gh_search
#'
#' @examples
#' \dontrun{
#' # Find highest concentration with a precision of a grid of 25 meters
#' hc1 <- highest_concentration(Groningen, amount, radius = 200,
#'  grid_distance = 25)
#'
#' # Increase the number of calls for more extensive search
#' hc1_nghb <- neighborhood_gh_search(hc1, max.call = 7000, highest_geohash = 1)
#' hc2_nghb <- neighborhood_gh_search(hc1, max.call = 7000, highest_geohash = 2)
#' plot(hc1_nghb)
#' plot(hc2_nghb)
#' }
neighborhood_gh_search <- function(hc, highest_geohash = 1, max.call = 1000,
                                   verbose = TRUE, seed = 1) {

  lifecycle::deprecate_warn(
    when = "0.7.2",
    what = "neighborhood_gh_search()",
    details = "Please use `find_highest_concentration()` instead."
  )

  if (!requireNamespace("geohashTools", quietly = TRUE)) {
    stop("geohashTools is needed for this function to work. Install it via
         install.packages(\"geohashTools\")", call. = FALSE)
  }

  if (!requireNamespace("GenSA", quietly = TRUE)) {
    stop("GenSA is needed for this function to work. Install it via
         install.packages(\"GenSA\")", call. = FALSE)
  }

  if (!inherits(hc, c("conc"))) {
    stop("Input must be of class conc
         Use highest_concentration() first.", call. = FALSE)
  }

  if (length(unique(hc$geohash)) < highest_geohash) {
    highest_geohash <- length(unique(hc$geohash))
    warning("highest_geohash must be equal or lower than the length of unique
            input geohashes. highest_geohash is set to ",
            highest_geohash, call. = FALSE)
  }

  value_nm <- attr(hc, "value_nm")
  pts_remaining <- attr(hc, "pts_remaining")
  radius <- attr(hc, "radius")

  names(pts_remaining)[names(pts_remaining) == value_nm] <- "valueconc"

  geohash <- unique(hc$geohash)[seq_len(highest_geohash)]

  # Function to minimize
  csa <- function(x) {
    df <- data.frame(lon = x[1], lat = x[2])
    -concentration(df, pts_remaining, valueconc, radius = radius,
                   display_progress = FALSE)$concentration[1]
  }

  gsa_lst <- vector("list", length(geohash))

  for (i in seq_along(geohash)){

    hash_dec <- geohashTools::gh_decode(geohash[i], include_delta = TRUE)

    lat_begin <- hash_dec$latitude - hash_dec$delta_latitude
    lat_end <- hash_dec$latitude + hash_dec$delta_latitude
    lon_begin <- hash_dec$longitude - hash_dec$delta_longitude
    lon_end <- hash_dec$longitude + hash_dec$delta_longitude

    # Lower and upper bounds for neighborhood search
    lower <- c(lon_begin, lat_begin)
    upper <- c(lon_end, lat_end)

    # Generate coordinates
    set.seed(seed)
    sa <- GenSA::GenSA(lower = lower, upper = upper,
                       fn = csa,
                       control = list(verbose = verbose,
                                      max.call = max.call))

    df <- data.frame(highest_concentration = -sa$value,
                     lon = sa$par[1],
                     lat = sa$par[2],
                     geohash = geohash[i])
    gsa_lst[[i]] <- df
  }

  names(pts_remaining)[names(pts_remaining) == "valueconc"] <- value_nm

  dc <- do.call(rbind, gsa_lst)
  attr(dc, "pts_remaining") <- pts_remaining
  attr(dc, "radius") <- radius
  attr(dc, "value_nm") <- value_nm

  class(dc) <- append("neighborhood", class(dc))
  dc
}


#' Automatically create a plot for objects obtained from highest_concentration()
#'
#' @description Takes an object produced by `highest_concentration()`,
#' and creates an interactive map.
#'
#' @param x object of class `conc` obtained from
#' `highest_concentration()`
#' @param grid_points show grid points (TRUE), or objects (FALSE)
#' @param legend_title title of legend
#' @param palette palette for grid points (defaults to "viridis")
#' @param legend_position legend position for grid points legend (defaults to
#' "bottomleft")
#' @param providers providers to show. See `leaflet::providers` for a list.
#' @param ... additional arguments affecting the interactive map produced
#'
#' @return Interactive view of geohashes with highest concentrations
#'
#' @author Martin Haringa
#'
#' @importFrom sf st_as_sf
#'
#' @export
plot.conc <- function(x,
                      grid_points = TRUE,
                      legend_title = NULL,
                      palette = "viridis",
                      legend_position = "bottomleft",
                      providers = c("CartoDB.Positron", "nlmaps.luchtfoto"),
                      ...) {

  if (!inherits(x, "conc")) {
    stop("plot.conc requires an object of class conc",
         call. = FALSE)
  }

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("leaflet is needed for this function to work. Install it via
         install.packages(\"leaflet\")", call. = FALSE)
  }

  if (!requireNamespace("leafem", quietly = TRUE)) {
    stop("leafem is needed for this function to work. Install it via
         install.packages(\"leafem\")", call. = FALSE)
  }

  if (!requireNamespace("leafgl", quietly = TRUE)) {
    stop("leafgl is needed for this function to work. Install it via
         install.packages(\"leafgl\")", call. = FALSE)
  }

  if (!requireNamespace("colourvalues", quietly = TRUE)) {
    stop("colourvalues is needed for this function to work. Install it via
         install.packages(\"colourvalues\")", call. = FALSE)
  }

  pts <- attr(x, "pts_remaining")
  geohashes0 <- attr(x, "gh_remaining")
  value_nm <- attr(x, "value_nm")
  legend_nm <- value_nm
  x_nm <- deparse(substitute(x))

  if (!is.null(legend_title)) {
    legend_nm <- legend_title
  }

  if (!isTRUE(grid_points)) {
    pts_sf <- sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
  }

  if (isTRUE(grid_points)) {

    if (!all(c("lon", "lat", "concentration") %in% names(x))) {
      stop(x_nm, " must contain columns `concentration`, `lon` and `lat`",
           call. = FALSE)
    }

    legend_nm <- "Concentration"
    value_nm <- "concentration"
    pts_sf <- sf::st_as_sf(x, coords = c("lon", "lat"), crs = 4326)
  }

  geohashes <- geohashes0[
    , north := latitude + delta_latitude
  ][
    , south := latitude - delta_latitude
  ][
    , east := longitude + delta_longitude
  ][
    , west := longitude - delta_longitude
  ]

  cols <- colourvalues::colour_values(pts_sf[[value_nm]], palette = palette)
  qpal <- leaflet::colorNumeric(palette, pts_sf[[value_nm]])

  ml <- leaflet::leaflet() |>

    # Base groups
    leaflet::addTiles(group = "OSM")

  ml <- add_providers_to_map(ml, providers)

  ml[["map"]] |>

    # Overlay groups
    leaflet::addRectangles(lng1 = geohashes$west,
                           lat1 = geohashes$south,
                           lng2 = geohashes$east,
                           lat2 = geohashes$north,
                           label = geohashes$geohash,
                           opacity = 1,
                           weight = 2,
                           fillOpacity = 0,
                           group = "Geohash") |>
    leafgl::addGlPoints(data = pts_sf,
                        fillColor = cols,
                        popup = TRUE,
                        group = "Points") |>
    leaflet::addLegend(data = pts_sf,
                       pal = qpal,
                       title = value_nm,
                       values = pts_sf[[value_nm]],
                       position = legend_position,
                       group = "Points") |>
    leafem::addMouseCoordinates() |>

    # Layers control
    leaflet::addLayersControl(
      baseGroups = c("OSM", ml[["used_providers"]]),
      overlayGroups = c("Points", "Geohash"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::hideGroup("Geohash")
}



#' Automatically create a plot for objects obtained from
#' neighborhood_gh_search()
#'
#' @description Takes an object produced by `neighborhood_gh_search()`, and
#' creates an interactive map.
#'
#' @param x object neighborhood object produced by `neighborhood_gh_search()`
#' @param buffer numeric value, show objects within buffer (in meters) from
#' circle (defaults to 0)
#' @param legend_title title of legend
#' @param palette palette for points (defaults to "viridis")
#' @param legend_position legend position for points legend (defaults to
#' "bottomleft")
#' @param palette_circle palette for circles (default to "YlOrRd")
#' @param legend_position_circle legend position for circles legend (defaults
#' to "bottomright")
#' @param legend_title_circle title of legend for circles
#' @param providers providers to show. See `leaflet::providers` for a list.
#' @param ... additional arguments affecting the interactive map produced
#'
#' @return Interactive view of highest concentration on map
#'
#' @author Martin Haringa
#'
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#' @importFrom sf st_buffer
#' @importFrom sf st_geometry
#'
#' @rdname plot
#'
#' @export
plot.neighborhood <- function(x,
                              buffer = 0,
                              legend_title = NULL,
                              palette = "viridis",
                              legend_position = "bottomleft",
                              palette_circle = "YlOrRd",
                              legend_position_circle = "bottomright",
                              legend_title_circle = "Highest concentration",
                              providers = c("CartoDB.Positron",
                                            "nlmaps.luchtfoto"),
                              ...) {

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("leaflet is needed for this function to work. Install it via
         install.packages(\"leaflet\")", call. = FALSE)
  }

  if (!requireNamespace("leafem", quietly = TRUE)) {
    stop("leafem is needed for this function to work. Install it via
         install.packages(\"leafem\")", call. = FALSE)
  }

  if (!inherits(x, "neighborhood")) {
    stop("plot.neighborhood requires an object of class neighborhood",
         call. = FALSE)
  }

  pts0 <- attr(x, "pts_remaining")
  radius <- attr(x, "radius")
  value_nm <- attr(x, "value_nm")
  legend_nm <- value_nm

  if (!is.null(legend_title)) {
    legend_nm <- legend_title
  }

  xgh <- unique(x[["geohash"]])
  gh_center_lst <- geohashTools::gh_decode(xgh, include_delta = TRUE)
  gh_center0 <- data.table::as.data.table(gh_center_lst)
  gh_center0$geohash <- xgh

  gh_center <- gh_center0[
    , north := latitude + delta_latitude
  ][
    , south := latitude - delta_latitude
  ][
    , east := longitude + delta_longitude
  ][
    , west := longitude - delta_longitude
  ]

  pts_lst <- vector("list", nrow(x))
  for (i in seq_len(nrow(x))) {
    pts_lst[[i]] <- points_in_circle(pts0,
                                     lon_center = x$lon[i],
                                     lat_center = x$lat[i],
                                     radius = radius + buffer)
  }

  pts <- do.call(rbind, pts_lst)
  pts_sf <- sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)

  circle_sf <- x |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(3035) |>
    sf::st_buffer(dist = units::set_units(radius, "meters")) |>
    sf::st_transform(4326)

  circle_sf$idrow <- seq_len(nrow(circle_sf))

  pal_points <- leaflet::colorNumeric(palette, pts_sf[[value_nm]])
  pal_circles <- leaflet::colorNumeric(palette_circle,
                                       circle_sf[["highest_concentration"]])

  popuptxt <- NULL
  for (i in seq_len(nrow(pts))) {
    x <- paste0("<b>", names(pts)[i], ": ", "</b>",
                pts[[names(pts)[i]]], "<br>")
    popuptxt <- paste(popuptxt, x)
  }

  m <- leaflet::leaflet() |>

    # Base groups
    leaflet::addTiles(group = "OSM")

  ml <- add_providers_to_map(m, providers)

  # Overlay groups with 200m circles
  m <- ml[["map"]] |>
    leaflet::addPolygons(data = circle_sf,
                         color = "red",
                         label = paste0(
                           format(circle_sf[["highest_concentration"]],
                                  decimal.mark = ".", big.mark = " "),
                           " (", circle_sf[["idrow"]], ")"
                         ),
                         fillColor = pal_circles(
                           circle_sf[["highest_concentration"]]
                         ),
                         labelOptions = leaflet::labelOptions(
                           noHide = TRUE,
                           textsize = "15px"
                         ),
                         stroke = TRUE,
                         weight = 1,
                         group = "Circle") |>

    # Overlay groups with geohashes
    leaflet::addRectangles(lng1 = gh_center$west,
                           lat1 = gh_center$south,
                           lng2 = gh_center$east,
                           lat2 = gh_center$north,
                           label = gh_center$geohash,
                           opacity = 1,
                           weight = 2,
                           fillOpacity = 0,
                           group = "Geohash") |>

    # Overlay groups with points
    leaflet::addCircleMarkers(data = pts_sf,
                              color = pal_points(pts_sf[[value_nm]]),
                              stroke = TRUE,
                              popup = popuptxt,
                              group = "Points") |>
    leaflet::addLegend(data = pts_sf,
                       pal = pal_points,
                       title = value_nm,
                       values = pts_sf[[value_nm]],
                       position = legend_position,
                       group = "Points") |>
    leafem::addMouseCoordinates() |>

    # Layers control
    leaflet::addLayersControl(
      baseGroups = c("OSM", ml[["used_providers"]]),
      overlayGroups = c("Points", "Circle", "Geohash"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::hideGroup("Geohash")

  if (nrow(circle_sf) > 1) {

    m <- m |>
      leaflet::addLegend(data = circle_sf,
                         pal = pal_circles,
                         title = legend_title_circle,
                         position = legend_position_circle,
                         values = circle_sf[["highest_concentration"]],
                         group = "Circle")
  }

  m
}
