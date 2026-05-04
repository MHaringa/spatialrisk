#' Example insurance portfolio
#'
#' @description A sample insurance portfolio with postal codes, insured amounts,
#' population at 4-digit postcode level, and point coordinates in EPSG:4326.
#'
#' @format A data frame with 29,990 rows and 5 variables:
#' \describe{
#'   \item{postcode}{6-digit postal code.}
#'   \item{population_pc4}{Population for the corresponding 4-digit postcode
#'   area.}
#'   \item{amount}{Insured amount.}
#'   \item{lon}{Longitude of the corresponding 6-digit postal code.}
#'   \item{lat}{Latitude of the corresponding 6-digit postal code.}
#' }
#'
#' @details This dataset is intended for examples and tests of spatial
#' concentration workflows.
#'
#' @author Martin Haringa
"insurance"
