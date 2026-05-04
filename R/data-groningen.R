#' Example addresses in Groningen
#'
#' @description A sample of addresses in Groningen with point coordinates in
#' EPSG:4326 and an example numeric amount column.
#'
#' @format A data frame with 25,000 rows and 9 variables:
#' \describe{
#'   \item{street}{Street name.}
#'   \item{number}{House number.}
#'   \item{letter}{House letter.}
#'   \item{suffix}{House number suffix.}
#'   \item{postal_code}{Postal code.}
#'   \item{city}{City name.}
#'   \item{lon}{Longitude.}
#'   \item{lat}{Latitude.}
#'   \item{amount}{Example numeric amount.}
#' }
#'
#' @details The \code{amount} column is an example value used in package examples
#' and tests.
#'
#' @source BAG, the Dutch registry for addresses and buildings
#' (Basisregistratie Adressen en Gebouwen), adapted for package examples.
"Groningen"
