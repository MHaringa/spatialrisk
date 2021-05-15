#' Highest concentration
#'
#' @description Find the centre coordinates of a circle with a fixed radius that maximizes the coverage of total fire risk insured.
#'
#' @param df data.frame of locations, should at least include column for longitude, latitude and sum insured
#' @param value column name with value of interest to summarize (e.g. sum insured)
#' @param lon column name with longitude (defaults to `lon`)
#' @param lat column name with latitude (defaults to `lat`)
#' @param lowerbound set lower bound for outcome (defaults to NULL)
#' @param radius radius (in meters) (default is 200m)
#' @param grid_distance distance (in meters) for precision of concentration risk (default is 20m).
#'     `neighborhood_search()` can be used to search for coordinates with even higher concentrations
#'     in the neighborhood of the highest concentrations.
#' @param gh_precision positive integer to define geohash precision. The precision controls the 'zoom level'.
#'     5 is 4.89 x 4.89km; 6 is 1.22km x 0.61km; 7 is 153m x 153m; 8 is 39m x 19m. Defaults to 6. Precision level
#'     of 6 must be used for a radius of 200m.
#'
#' @import data.table
#' @import geohashTools
#'
#' @author Martin Haringa
#'
#' @return data.frame
#' @examples
#'  \dontrun{
#' # Find highest concentration with a precision of a grid of 20 meters
#' hc1 <- highest_concentration(Groningen, amount, radius = 200, grid_distance = 20)
#'
#' # Set distance between grid points to 10 meters
#' hc2 <- highest_concentration(Groningen, amount, radius = 200, grid_distance = 10)
#'
#' # Increase the number of calls to the concentration function for more extensive search
#' hc1_nghb <- neighborhood_gh_search(hc1, max.call = 7000)
#' hc2_nghb <- neighborhood_gh_search(hc2, max.call = 7000)
#' print(hc1_nghb)
#' print(hc2_nghb)
#' }
#'
#' @export
highest_concentration <- function(df, value, lon = lon, lat = lat, lowerbound = NULL, radius = 200, grid_distance = 20,
                                  gh_precision = 6, display_progress = TRUE){

  value_nm <- deparse(substitute(value))
  lon_nm <- deparse(substitute(lon))
  lat_nm <- deparse(substitute(lat))

  # Add geohash (length 5 is 4.89km x 4.89km; length 6 is 1.22 x 0.61km; length 7 is 153m x 153m; length 8 is 38m x 19m)
  df[["geohash"]] <- geohashTools::gh_encode(latitude = df[[lat_nm]],
                                          longitude = df[[lon_nm]],
                                          precision = gh_precision)

  # Determine lower bound for concentration (based on circles around 1000 highest sums insured)
  # This only works with some relatively high sums insured
  if ( is.null(lowerbound) ){
    lowerbound_1 <- lower_bound_fn(df, value_nm, radius = radius, highest = 1000)
  }

  portfolio_dt <- data.table::data.table(df)

  # Calculate total sum insured per geohash
  gh_sum <- portfolio_dt[, .(gh_self_sum = sum(get(value_nm))), by = geohash]

  # Determine 8 neighbors of geohash and calculate total sum insured for the 9 geohashes
  gh_sum_nghbrs <- add_gh_nghbrs_sum(gh_sum, "gh_self_sum")

  # Find lowerbound for use cases where sum insured is same for all coords
  lowerbound_2 <- lower_bound_fn2(gh_sum_nghbrs, portfolio_dt, value_nm, radius = radius)

  # Take max of both lowerbounds
  lowerbound <- max(lowerbound_1, lowerbound_2)

  # Remove geohashes with a total sum insured lower than the lower bound
  gh_remaining <- gh_sum_nghbrs[gh_nghbrs_sum >= lowerbound]

  # Coordinates or remaining points in portfolio within remaining geohashes
  pts_remaining <- portfolio_dt[geohash %in% gh_remaining$geohash]
  names(pts_remaining)[names(pts_remaining) == value_nm] <- "_sum_insured"

  # Determine centre point for each remaining geohash (delta is distance to bound)
  gh_center_lst <- geohashTools::gh_decode(gh_remaining$geohash,
                                           include_delta = TRUE)
  gh_center <- data.table::as.data.table(gh_center_lst)

  # Determine sum of remaining points within bounding box of radius
  # around remaining geohashes
  bbox_sum <- add_gh_bbox_sum(gh_center, pts_remaining, radius = radius)
  gh_sum_bbox <- cbind(gh_remaining, bbox_sum)

  # Remove (again) geohashes with a total sum insured lower than the lower bound
  gh_remaining_bbox <- gh_sum_bbox[sum_bbox >= lowerbound]

  # Coordinates or remaining points in portfolio within remaining geohashes
  pts_remaining2 <- portfolio_dt[geohash %in% gh_remaining_bbox$geohash]

  # Create grid points in remaining geohashes
  gh_grid <- create_grid_points(gh_remaining_bbox, meters = grid_distance)

  # Concentration for each grid point
  gh_grid_conc <- concentration(gh_grid, pts_remaining, `_sum_insured`,
                                radius = radius,
                                display_progress = display_progress)

  gh_grid_conc_sort <- gh_grid_conc[order(-concentration)]

  attr(gh_grid_conc_sort, "value_nm") <- value_nm
  attr(gh_grid_conc_sort, "pts_remaining") <- pts_remaining2
  attr(gh_grid_conc_sort, "gh_remaining") <- gh_remaining_bbox
  attr(gh_grid_conc_sort, "radius") <- radius

  class(gh_grid_conc_sort) <- append(class(gh_grid_conc_sort), "concentration")
  return(gh_grid_conc_sort)
}


#' Search for coordinates with higher concentrations within geohash
#'
#' `highest_concentration()` returns the highest concentration within a portfolio based on a grid.
#' The distance (in meters) within the points in the grid can be set by the `grid_distance` argument.
#' However, higher concentrations can be found within two grid points. `neighborhood_gh_search()`
#' looks for even higher concentrations in the neighborhood of the grid points with the highest
#' concentrations. This optimization is done by means of Simulated Annealing.
#'
#' @param hc object of class `concentration` obtained from `highest_concentration()`
#' @param geohash character vector with the name of the geohash (defaults to NULL)
#' @param highest_geohash the number of geohashes the searching algorithm is applied to.
#'     Defaults to 1 (i.e. algorithm is only applied to the geohash with the highest concentration).
#' @param max.call maximum number of calls to the concentration function
#'     (i.e. the number of coordinates in the neighborhood of the highest concentration). Defaults to 1000.
#' @param verbose show messages from the algorithm (TRUE/FALSE). Defaults to FALSE.
#'
#' @importFrom GenSA GenSA
#' @importFrom geohashTools gh_decode
#'
#' @author Martin Haringa
#'
#' @return data.frame
#' @examples
#' \dontrun{
#' # Find highest concentration with a precision of a grid of 20 meters
#' hc1 <- highest_concentration(Groningen, amount, radius = 200, grid_distance = 20)
#'
#' # Set distance between grid points to 10 meters
#' hc2 <- highest_concentration(Groningen, amount, radius = 200, grid_distance = 10)
#'
#' # Increase the number of calls to the concentration function for more extensive search
#' hc1_nghb <- neighborhood_gh_search(hc1, max.call = 7000)
#' hc2_nghb <- neighborhood_gh_search(hc2, max.call = 7000)
#' print(hc1_nghb)
#' print(hc2_nghb)
#' }
#'
#' @export
neighborhood_gh_search <- function(hc, highest_geohash = 1, max.call = 1000, verbose = TRUE){

  if( !inherits(hc, c("concentration")) ) {
    stop("Input must be of class concentration. Use highest_concentration() first.", call. = FALSE)
  }

  value_nm <- attr(hc, "value_nm")
  pts_remaining <- attr(hc, "pts_remaining")
  radius <- attr(hc, "radius")

  names(pts_remaining)[names(pts_remaining) == value_nm] <- "valueconc"

  geohash <- unique(hc$geohash)[1:highest_geohash]

  # Function to minimize
  csa <- function(x){
    df <- data.frame(lon = x[1], lat = x[2])
    -concentration(df, pts_remaining, valueconc, display_progress = FALSE)$concentration[1]
  }

  gsa_lst <- vector("list", length(geohash))

  for (i in 1:length(geohash)){

    hash_dec <- geohashTools::gh_decode(geohash[i], include_delta = TRUE)

    lat_begin <- hash_dec$latitude - hash_dec$delta_latitude
    lat_end <- hash_dec$latitude + hash_dec$delta_latitude
    lon_begin <- hash_dec$longitude - hash_dec$delta_longitude
    lon_end <- hash_dec$longitude + hash_dec$delta_longitude

    # Lower and upper bounds for neighborhood search
    lower <- c(lon_begin, lat_begin)
    upper <- c(lon_end, lat_end)

    # Generate coordinates
    sa <- GenSA::GenSA(lower = lower, upper = upper,
                       fn = csa,
                       control = list(verbose = verbose,
                                      max.call = max.call))

    df <- data.frame(highest_concentration = -sa$value, lon = sa$par[1], lat = sa$par[2],
                     geohash = geohash[i])
    gsa_lst[[i]] <- df
  }

  names(pts_remaining)[names(pts_remaining) == "valueconc"] <- value_nm

  dc <- do.call(rbind, gsa_lst)
  attr(dc, "pts_remaining") <- pts_remaining
  attr(dc, "radius") <- radius
  return(dc)
}


#' Test 2
#'
#' @param hc highest concentration
#'
#' @return sdf
#' @author Martin Haringa
#'
#' @import data.table
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @import sf
#' @import mapview
#'
#' @export
f1 <- function(hc){

  pts <- attr(hc, "pts_remaining")
  geohashes0 <- attr(hc, "gh_remaining")
  pts_sf <- sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)

  geohashes <- geohashes0[
    , north := latitude + delta_latitude][
      , south := latitude - delta_latitude][
        , east := longitude + delta_longitude][
          , west := longitude - delta_longitude]

  ne <- geohashes[, .(geohash, lon = east, lat = north)]
  nw <- geohashes[, .(geohash, lon = west, lat = north)]
  sw <- geohashes[, .(geohash, lon = west, lat = south)]
  se <- geohashes[, .(geohash, lon = east, lat = south)]

  dir_comb <- rbind(ne, nw, sw, se)
  dir_comb_sf <- sf::st_as_sf(dir_comb, coords = c("lon", "lat"), crs = 4326)

  rect_sf <- dir_comb_sf %>%
    dplyr::group_by(geohash) %>%
    dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
    sf::st_cast("POLYGON")

  mapview::mapview(rect_sf, col.regions = "skyblue", legend = FALSE,
                   alpha.regions = .2) +
    mapview::mapview(pts_sf, zcol = "amount")
}

#' Test1
#'
#' @param hc object
#'
#' @return Interactive view of highest concentration on top of base map
#'
#' @author Martin Haringa
#'
#' @import sf
#' @import mapview
#'
#' @export
f2 <- function(hc){
  pts0 <- attr(hc, "pts_remaining")
  radius <- attr(hc, "radius")

  pts_lst <- vector("list", nrow(hc))
  for ( i in 1:nrow(hc)){
    pts_lst[[i]] <- points_in_circle(pts0,
                                     lon_center = hc$lon[i],
                                     lat_center = hc$lat[i],
                                     radius = radius + 200)
  }

  pts <- do.call(rbind, pts_lst)
  pts_sf <- sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)

  circle_sf <- hc %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    sf::st_transform(3035) %>%
    sf::st_buffer(dist = units::set_units(radius, "meters")) %>%
    sf::st_transform(4326) %>%
    sf::st_geometry()

  mapview::mapview(pts_sf, zcol = "amount") +
    mapview::mapview(circle_sf, col.regions = "red")
}


