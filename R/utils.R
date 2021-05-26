
#' Determine a lower bound for highest concentration
#' Calculate concentration for the coordinates of the highest insured sums
#'
#' @importFrom utils head
#'
#' @keywords internal
lower_bound_fn <- function(df, col, lat_nm, lon_nm, radius = 200, highest = 50){

  df_order <- df[order(-df[[col]]),]
  high_df <- head(df_order, highest)
  high_df_pts <- high_df[,c(lon_nm, lat_nm)]
  colnames(high_df_pts) <- c("lon", "lat")

  names(df)[names(df) == col] <- "valueconc"
  names(df)[names(df) == lat_nm] <- "lat"
  names(df)[names(df) == lon_nm] <- "lon"

  conc <- concentration(high_df_pts, df, valueconc,
                        display_progress = FALSE,
                        radius = radius)
  max(conc$concentration)
}

#' @importFrom utils head
#' @importFrom data.table setorder
#'
#' @keywords internal
lower_bound_fn2 <- function(df, full, col, lat_nm, lon_nm,
                            radius = 200, highest = 100){

  df_order <- data.table::setorder(df, -geohash_sum)
  high_df <- head(df_order, highest)
  high_df_gh_lst <- geohashTools::gh_decode(high_df$geohash)
  high_df_gh <- data.frame(high_df_gh_lst)
  names(high_df_gh) <- c("lat", "lon")
  names(full)[names(full) == col] <- "valueconc"
  names(full)[names(full) == lat_nm] <- "lat"
  names(full)[names(full) == lon_nm] <- "lon"
  conc <- concentration(high_df_gh, full, valueconc,
                        display_progress = FALSE,
                        radius = radius)
  max(conc$concentration)
}

#' Determine total insured sum of all neighbours (including hash itself)
#' df should contain a column called hash and a column with sum per hash
#'
#' @importFrom data.table setDT
#' @importFrom data.table setnames
#' @importFrom data.table melt
#' @importFrom data.table setkey
#'
#' @keywords internal
add_gh_nghbrs_sum <- function(df, colname_sum){

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
                                     measure.vars = c("southwest",
                                                      "south",
                                                      "southeast",
                                                      "west",
                                                      "east",
                                                      "northwest",
                                                      "north",
                                                      "northeast",
                                                      "copy_self"))
  data.table::setnames(x_dt, old = "geohash", new = "value")
  data.table::setkey(gh_nghbrs_long, "value")

  # Aggregate sum of hash
  agg_sum <- gh_nghbrs_long[x_dt,][,.(gh_nghbrs_sum = sum(geohash_sum)), by = "geohash"]

  # Add original sum
  data.table::setkey(agg_sum, "geohash")
  agg_sum[x_dt,][]
}


#' Determine total sum for each geohash with buffer of 200m
#'
#' @param hash_decode data.frame as output from `geohashTools::gh_decode()`
#' @param full data.frame with portfolio with lon, lat and sum insured
#'
#' @useDynLib spatialrisk
#' @importFrom Rcpp sourceCpp
#' @importFrom Rcpp evalCpp
#'
#' @keywords internal
add_gh_bbox_sum <- function(hash_decode, full, lon_nm, lat_nm, radius){

  sub_df <- data.frame("lon" = hash_decode[["longitude"]],
                       "lat" = hash_decode[["latitude"]],
                       "delta_latitude" = hash_decode[["delta_latitude"]],
                       "delta_longitude" = hash_decode[["delta_longitude"]])

  full_df <- data.frame("lon" = full[[lon_nm]],
                        "lat" = full[[lat_nm]],
                        "value" = full[["_sum_insured"]])

  block_df <- block_loop_cpp(sub_df, full_df, radius, display_progress = FALSE)
  hash_decode$sum_bbox <- block_df$cumulation

  return(hash_decode)
}



#'
#' @importFrom data.table rbindlist
#'
#' @keywords internal
create_grid_points <- function(df, meters){

  circumference_earth_in_meters <- 40075000
  one_lat_in_meters <- circumference_earth_in_meters * 0.002777778

  uit <- vector("list", nrow(df))
  for (i in 1:nrow(df)){
    lat_step <- seq(df[["latitude"]][i] - df[["delta_latitude"]][i],
                    df[["latitude"]][i] + df[["delta_latitude"]][i],
                    by = meters / one_lat_in_meters)
    one_lon_in_meters <- circumference_earth_in_meters * cos(df[["latitude"]][i] * 0.01745329) * 0.002777778
    lon_step <- seq(df[["longitude"]][i] - df[["delta_longitude"]][i],
                    df[["longitude"]][i] + df[["delta_longitude"]][i],
                    by = meters / one_lon_in_meters)
    eg <- expand.grid(lon = lon_step, lat = lat_step)
    eg[["geohash"]] <- df[["geohash"]][i]
    uit[[i]] <- eg
  }
  data.table::rbindlist(uit)
}


#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`
