
#' Determine a lower bound for highest concentration
#' Calculate concentration for the coordinates of the highest insured sums
#'
#' @importFrom utils head
#'
#' @keywords internal
lower_bound_fn <- function(df, col, radius = 200, highest = 50){

  if ( !all(c("lon", "lat") %in% names(df))) {
    stop("columns lat and lon do not exist in df", call. = FALSE)
  }

  df_order <- df[order(-df[[col]]),]
  high_df <- head(df_order, highest)
  high_df_pts <- high_df[,c("lon","lat")]
  names(df)[names(df) == col] <- "valueconc"
  conc <- concentration(high_df_pts, df, valueconc,
                        display_progress = FALSE,
                        radius = radius)
  max(conc$concentration)
}

#' @importFrom utils head
#'
#' @keywords internal
lower_bound_fn2 <- function(df, full, col, radius = 200, highest = 100){

  df_order <- data.table::setorder(df, -geohash_sum)
  high_df <- head(df_order, highest)
  high_df_gh_lst <- geohashTools::gh_decode(high_df$geohash)
  high_df_gh <- data.frame(high_df_gh_lst)
  names(high_df_gh) <- c("lat", "lon")
  names(full)[names(full) == col] <- "valueconc"
  conc <- concentration(high_df_gh, full, valueconc,
                        display_progress = FALSE,
                        radius = radius)
  max(conc$concentration)
}

#' Determine total insured sum of all neighbours (including hash itself)
#' df should contain a column called hash and a column with sum per hash
#'
#' @import data.table
#'
#' @keywords internal
add_gh_nghbrs_sum <- function(df, colname_sum){

  if(!"geohash" %in% colnames(df)){
    stop("df should contain column with hash", call. = FALSE)
  }

  x_dt <- data.table::setDT(df, key = "geohash")
  data.table::setnames(x_dt, old = colname_sum, new = "geohash_sum")

  # Geohash neighbors
  gh_nghbrs <- geohashTools::gh_neighbors(df$geohash)

  # From wide to long format
  gh_nghbrs$copy_self <- gh_nghbrs$self
  gh_nghbrs_dt <- data.table::setDT(gh_nghbrs)
  data.table::setnames(gh_nghbrs_dt, old = "self", new = "geohash")
  gh_nghbrs_long <- data.table::melt(gh_nghbrs_dt,
                                     id.vars = c("geohash"),
                                     measure.vars = c("southwest", "south",
                                                      "southeast", "west",
                                                      "east", "northwest",
                                                      "north", "northeast",
                                                      "copy_self"))
  data.table::setkey(gh_nghbrs_long, "geohash")

  # Aggregate sum of hash
  gh_nghbrs_long[x_dt,][,.(gh_nghbrs_sum = sum(geohash_sum)), by = "geohash"][x_dt,][]
}


#'
#' @param hash_decode data.frame as output from `geohashTools::gh_decode()`
#' @param full data.frame with portfolio with lon, lat and sum insured
#'
#' @useDynLib spatialrisk
#' @importFrom Rcpp sourceCpp
#' @importFrom Rcpp evalCpp
#'
#' @keywords internal
add_gh_bbox_sum <- function(hash_decode, full, radius){

  sub_df <- data.frame("lon" = hash_decode[["longitude"]],
                       "lat" = hash_decode[["latitude"]],
                       "delta_latitude" = hash_decode[["delta_latitude"]],
                       "delta_longitude" = hash_decode[["delta_longitude"]])

  full_df <- data.frame("lon" = full[["lon"]],
                        "lat" = full[["lat"]],
                        "value" = full[["_sum_insured"]])

  block_df <- block_loop_cpp(sub_df, full_df, radius, display_progress = FALSE)
  hash_decode$sum_bbox <- block_df$cumulation

  return(hash_decode)
}


#' @keywords internal
create_grid_points <- function(df, meters, hash = geohash,
                               lon = longitude, lat = latitude,
                               delta_lon = delta_longitude,
                               delta_lat = delta_latitude){

  hash_nm <- deparse(substitute(hash))
  lon_nm <- deparse(substitute(longitude))
  lat_nm <- deparse(substitute(latitude))
  delta_lat_nm <- deparse(substitute(delta_latitude))
  delta_lon_nm <- deparse(substitute(delta_longitude))

  circumference_earth_in_meters <- 40075000
  one_lat_in_meters <- circumference_earth_in_meters * 0.002777778

  uit <- vector("list", nrow(df))
  for (i in 1:nrow(df)){
    lat_step <- seq(df[[lat_nm]][i] - df[[delta_lat_nm]][i],
                    df[[lat_nm]][i] + df[[delta_lat_nm]][i],
                    by = meters / one_lat_in_meters)
    one_lon_in_meters <- circumference_earth_in_meters * cos(df[[lat_nm]][i] * 0.01745329) * 0.002777778
    lon_step <- seq(df[[lon_nm]][i] - df[[delta_lon_nm]][i],
                    df[[lon_nm]][i] + df[[delta_lon_nm]][i],
                    by = meters / one_lon_in_meters)
    eg <- expand.grid(lon = lon_step, lat = lat_step)
    eg[[hash_nm]] <- df[[hash_nm]][i]
    uit[[i]] <- eg
  }
  data.table::rbindlist(uit)
}


#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
