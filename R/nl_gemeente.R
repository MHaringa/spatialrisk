#' Object of class sf for gemeentes (municipalities) in the Netherlands
#'
#' @description An object of class sf (simple feature) for gemeentes (English: municipalities) in the Netherlands.
#'
#' @author Martin Haringa
#'
#' @format A simple feature object with 380 rows and 4 variables:
#' \describe{
#'   \item{id}{id of gemeente}
#'   \item{code}{code of gemeente}
#'   \item{gemeentena}{name of gemeente}
#'   \item{geometry}{geometry object of gemeente}
#' }
"nl_gemeente"

#' Object of class sf for provincies (provinces) in the Netherlands
#'
#' @description An object of class sf (simple feature) for provincies (English: provinces) in the Netherlands.
#'
#' @author Martin Haringa
#'
#' @format A simple feature object with 12 rows and 2 variables:
#' \describe{
#'   \item{province_name}{province name}
#'   \item{geometry}{geometry object of province}
#' }
"nl_provincie"

#' Object of class sf for COROP regions in the Netherlands
#'
#' @description An object of class sf (simple feature) for COROP regions in the Netherlands.
#'
#' @author Martin Haringa
#'
#' @details A COROP region is a regional area within the Netherlands. These regions are used for analytical purposes by, among others, Statistics Netherlands. The Dutch abbreviation stands for Coordinatiecommissie Regionaal Onderzoeksprogramma, literally the Coordination Commission Regional Research Programme.
#'
#' @format A simple feature object with 40 rows and 3 variables:
#' \describe{
#'   \item{corop_nr}{corop number}
#'   \item{corop_name}{corop name}
#'   \item{geometry}{geometry object of COROP region}
#' }
"nl_corop"

#' Object of class sf for 1-digit postcode regions in the Netherlands
#'
#' @description An object of class sf (simple feature) for 1-digit postcode (English: postal code) regions in the Netherlands.
#'
#' @author Martin Haringa
#'
#' @details Postal codes in the Netherlands, known as postcodes, are alphanumeric, consisting of four digits followed by two uppercase letters. The first two digits indicate a city and a region, the second two digits and the two letters indicate a range of house numbers, usually on the same street.
#'
#' @format A simple feature object with 9 rows and 2 variables:
#' \describe{
#'   \item{pc1}{1-digit postal code}
#'   \item{geometry}{geometry object of postal code}
#' }
"nl_postcode1"

#' Object of class sf for 2-digit postcode regions in the Netherlands
#'
#' @description An object of class sf (simple feature) for 2-digit postcode (English: postal code) regions in the Netherlands.
#'
#' @author Martin Haringa
#'
#' @details Postal codes in the Netherlands, known as postcodes, are alphanumeric, consisting of four digits followed by two uppercase letters. The first two digits indicate a city and a region, the second two digits and the two letters indicate a range of house numbers, usually on the same street.
#'
#' @format A simple feature object with 90 rows and 2 variables:
#' \describe{
#'   \item{pc2}{2-digit postal code}
#'   \item{geometry}{geometry object of postal code}
#' }
"nl_postcode2"

#' Object of class sf for 3-digit postcode regions in the Netherlands
#'
#' @description An object of class sf (simple feature) for 3-digit postcode (English: postal code) regions in the Netherlands.
#'
#' @author Martin Haringa
#'
#' @details Postal codes in the Netherlands, known as postcodes, are alphanumeric, consisting of four digits followed by two uppercase letters. The first two digits indicate a city and a region, the second two digits and the two letters indicate a range of house numbers, usually on the same street.
#'
#' @format A simple feature object with 799 rows and 2 variables:
#' \describe{
#'   \item{pc3}{3-digit postal code}
#'   \item{geometry}{geometry object of postal code}
#' }
"nl_postcode3"

#' Object of class sf for 4-digit postcode regions in the Netherlands
#'
#' @description An object of class sf (simple feature) for 4-digit postcode (English: postal code) regions in the Netherlands.
#'
#' @author Martin Haringa
#'
#' @details Postal codes in the Netherlands, known as postcodes, are alphanumeric, consisting of four digits followed by two uppercase letters. The first two digits indicate a city and a region, the second two digits and the two letters indicate a range of house numbers, usually on the same street.
#'
#' @format A simple feature object with 4053 rows and 2 variables:
#' \describe{
#'   \item{pc4}{4-digit postal code}
#'   \item{pc4_name}{name of corresponding 4-digit postal code}
#'   \item{geometry}{geometry object of postal code}
#' }
"nl_postcode4"

#' Object of class sf for countries of the entire world
#'
#' @description An object of class sf (simple feature) for countries of the entire world.
#'
#' @author Martin Haringa
#'
#' @format A simple feature object with 234 rows and 29 variables.
"world_countries"

#' Object of class sf for countries of Europe
#'
#' @description An object of class sf (simple feature) for countries of Europe
#'
#' @author Martin Haringa
#'
#' @details The epsg (SRID) is set to 102013 (Europe Albers Equal Area Conic).
#'
#' @format A simple feature object with 51 rows and 29 variables.
"europe_countries"
