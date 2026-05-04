#' Municipalities in the Netherlands
#'
#' @description An \code{sf} object with municipal geometries for the
#' Netherlands in 2021. Centroid coordinates are included in EPSG:4326.
#'
#' @format A simple feature object with 380 rows and 6 variables:
#' \describe{
#'   \item{id}{Municipality identifier.}
#'   \item{code}{Municipality code.}
#'   \item{areaname}{Municipality name.}
#'   \item{lon}{Longitude of the municipality centroid.}
#'   \item{lat}{Latitude of the municipality centroid.}
#'   \item{geometry}{Municipality geometry.}
#' }
#'
#' @source Statistics Netherlands (CBS), adapted for package examples.
#' @author Martin Haringa
"nl_gemeente"

#' Provinces in the Netherlands
#'
#' @description An \code{sf} object with province geometries for the Netherlands.
#' Centroid coordinates are included in EPSG:4326.
#'
#' @format A simple feature object with 12 rows and 4 variables:
#' \describe{
#'   \item{areaname}{Province name.}
#'   \item{geometry}{Province geometry.}
#'   \item{lon}{Longitude of the province centroid.}
#'   \item{lat}{Latitude of the province centroid.}
#' }
#'
#' @source Statistics Netherlands (CBS), adapted for package examples.
#' @author Martin Haringa
"nl_provincie"

#' COROP regions in the Netherlands
#'
#' @description An \code{sf} object with COROP region geometries for the
#' Netherlands. Centroid coordinates are included in EPSG:4326.
#'
#' @details COROP regions are regional areas used for analytical purposes by,
#' among others, Statistics Netherlands.
#'
#' @format A simple feature object with 40 rows and 5 variables:
#' \describe{
#'   \item{corop_nr}{COROP number.}
#'   \item{areaname}{COROP region name.}
#'   \item{geometry}{COROP region geometry.}
#'   \item{lon}{Longitude of the COROP centroid.}
#'   \item{lat}{Latitude of the COROP centroid.}
#' }
#'
#' @source Statistics Netherlands (CBS), adapted for package examples.
#' @author Martin Haringa
"nl_corop"

#' Two-digit postcode regions in the Netherlands
#'
#' @description An \code{sf} object with 2-digit postcode region geometries for
#' the Netherlands. Centroid coordinates are included in EPSG:4326.
#'
#' @details Postal codes in the Netherlands are alphanumeric and consist of four
#' digits followed by two uppercase letters. This object aggregates those codes
#' to their first two digits.
#'
#' @format A simple feature object with 90 rows and 4 variables:
#' \describe{
#'   \item{areaname}{2-digit postcode area.}
#'   \item{geometry}{Postcode region geometry.}
#'   \item{lon}{Longitude of the 2-digit postcode centroid.}
#'   \item{lat}{Latitude of the 2-digit postcode centroid.}
#' }
#'
#' @source Adapted from Dutch postcode boundary data for package examples.
#' @author Martin Haringa
"nl_postcode2"

#' Three-digit postcode regions in the Netherlands
#'
#' @description An \code{sf} object with 3-digit postcode region geometries for
#' the Netherlands. Centroid coordinates are included in EPSG:4326.
#'
#' @details Postal codes in the Netherlands are alphanumeric and consist of four
#' digits followed by two uppercase letters. This object aggregates those codes
#' to their first three digits.
#'
#' @format A simple feature object with 799 rows and 4 variables:
#' \describe{
#'   \item{areaname}{3-digit postcode area.}
#'   \item{geometry}{Postcode region geometry.}
#'   \item{lon}{Longitude of the 3-digit postcode centroid.}
#'   \item{lat}{Latitude of the 3-digit postcode centroid.}
#' }
#'
#' @source Adapted from Dutch postcode boundary data for package examples.
#' @author Martin Haringa
"nl_postcode3"

#' Four-digit postcode regions in the Netherlands
#'
#' @description An \code{sf} object with 4-digit postcode region geometries for
#' the Netherlands. Centroid coordinates are included in EPSG:4326.
#'
#' @details Postal codes in the Netherlands are alphanumeric and consist of four
#' digits followed by two uppercase letters. This object aggregates those codes
#' to their first four digits.
#'
#' @format A simple feature object with 4053 rows and 7 variables:
#' \describe{
#'   \item{pc4}{4-digit postcode.}
#'   \item{areaname}{Name of corresponding 4-digit postcode area.}
#'   \item{city}{City name.}
#'   \item{biggest_20cities}{Whether the area is in one of the twenty largest
#'   cities in the Netherlands.}
#'   \item{geometry}{Postcode region geometry.}
#'   \item{lon}{Longitude of the 4-digit postcode centroid.}
#'   \item{lat}{Latitude of the 4-digit postcode centroid.}
#' }
#'
#' @source Adapted from Dutch postcode boundary data for package examples.
#' @author Martin Haringa
"nl_postcode4"
