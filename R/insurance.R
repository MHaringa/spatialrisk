#' Sum insured per postal code in the Netherlands
#'
#' @description A dataset of postal codes with their sum insured, population
#' and the corresponding spatial locations in terms of a latitude and a
#' longitude.
#'
#' @author Martin Haringa
#'
#' @format A data frame with 29,990 rows and 5 variables:
#' \describe{
#'   \item{postcode}{6-digit postal code}
#'   \item{population_pc4}{Population per 4-digit postal code}
#'   \item{amount}{Sum insured}
#'   \item{lon}{Longitude (in degrees) of the corresponding 6-digit postal code}
#'   \item{lat}{Latitude (in degrees) of the corresponding 6-digit postal code}
#' }
"insurance"
