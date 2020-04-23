#' Object of class \code{sf} for municipalities in the Netherlands
#'
#' @description An object of class \code{sf} (simple feature) for municipalities (Dutch: gemeentes) in the Netherlands in the year 2018.
#'
#' @author Martin Haringa
#'
#' @format A simple feature object with 380 rows and 6 variables:
#' \describe{
#'   \item{id}{id of gemeente}
#'   \item{code}{code of gemeente}
#'   \item{areaname}{name of gemeente}
#'   \item{geometry}{geometry object of gemeente}
#'   \item{lon}{longitude of the gemeente centroid}
#'   \item{lat}{latitude of the gemeente centroid}
#' }
"nl_gemeente"

#' Object of class \code{sf} for provinces in the Netherlands
#'
#' @description An object of class \code{sf} (simple feature) for provinces (Dutch: provincies) in the Netherlands.
#'
#' @author Martin Haringa
#'
#' @format A simple feature object with 12 rows and 4 variables:
#' \describe{
#'   \item{areaname}{province name}
#'   \item{geometry}{geometry object of province}
#'   \item{lon}{longitude of the province centroid}
#'   \item{lat}{latitude of the province centroid}
#' }
"nl_provincie"

#' Object of class \code{sf} for COROP regions in the Netherlands
#'
#' @description An object of class \code{sf} (simple feature) for COROP regions in the Netherlands.
#'
#' @author Martin Haringa
#'
#' @details A COROP region is a regional area within the Netherlands. These regions are used for analytical purposes by, among others, Statistics Netherlands. The Dutch abbreviation stands for Coordinatiecommissie Regionaal Onderzoeksprogramma, literally the Coordination Commission Regional Research Programme.
#'
#' @format A simple feature object with 40 rows and 5 variables:
#' \describe{
#'   \item{corop_nr}{corop number}
#'   \item{areaname}{corop name}
#'   \item{geometry}{geometry object of COROP region}
#'   \item{lon}{longitude of the corop centroid}
#'   \item{lat}{latitude of the corop centroid}
#' }
"nl_corop"

#' Object of class \code{sf} for 1-digit postcode regions in the Netherlands
#'
#' @description An object of class \code{sf} (simple feature) for 1-digit postal codes (Dutch: postcode) regions in the Netherlands.
#'
#' @author Martin Haringa
#'
#' @details Postal codes in the Netherlands, known as postcodes, are alphanumeric, consisting of four digits followed by two uppercase letters. The first two digits indicate a city and a region, the second two digits and the two letters indicate a range of house numbers, usually on the same street.
#'
#' @format A simple feature object with 9 rows and 4 variables:
#' \describe{
#'   \item{areaname}{1-digit postal code}
#'   \item{geometry}{geometry object of postal code}
#'   \item{lon}{longitude of the 1-digit postal code centroid}
#'   \item{lat}{latitude of the 1-digit postal code centroid}
#' }
"nl_postcode1"

#' Object of class \code{sf} for 2-digit postcode regions in the Netherlands
#'
#' @description An object of class \code{sf} (simple feature) for 2-digit postal codes (Dutch: postcode) regions in the Netherlands.
#'
#' @author Martin Haringa
#'
#' @details Postal codes in the Netherlands, known as postcodes, are alphanumeric, consisting of four digits followed by two uppercase letters. The first two digits indicate a city and a region, the second two digits and the two letters indicate a range of house numbers, usually on the same street.
#'
#' @format A simple feature object with 90 rows and 4 variables:
#' \describe{
#'   \item{areaname}{2-digit postal code}
#'   \item{geometry}{geometry object of postal code}
#'   \item{lon}{longitude of the 2-digit postal code centroid}
#'   \item{lat}{latitude of the 2-digit postal code centroid}
#' }
"nl_postcode2"

#' Object of class \code{sf} for 3-digit postcode regions in the Netherlands
#'
#' @description An object of class \code{sf} (simple feature) for 3-digit postal codes (Dutch: postcode) regions in the Netherlands.
#'
#' @author Martin Haringa
#'
#' @details Postal codes in the Netherlands, known as postcodes, are alphanumeric, consisting of four digits followed by two uppercase letters. The first two digits indicate a city and a region, the second two digits and the two letters indicate a range of house numbers, usually on the same street.
#'
#' @format A simple feature object with 799 rows and 3 variables:
#' \describe{
#'   \item{areaname}{3-digit postal code}
#'   \item{geometry}{geometry object of postal code}
#'   \item{lon}{longitude of the 3-digit postal code centroid}
#'   \item{lat}{latitude of the 3-digit postal code centroid}
#' }
"nl_postcode3"

#' Object of class \code{sf} for 4-digit postcode regions in the Netherlands
#'
#' @description An object of class \code{sf} (simple feature) for 4-digit postal codes (Dutch: postcode) regions in the Netherlands.
#'
#' @author Martin Haringa
#'
#' @details Postal codes in the Netherlands, known as postcodes, are alphanumeric, consisting of four digits followed by two uppercase letters. The first two digits indicate a city and a region, the second two digits and the two letters indicate a range of house numbers, usually on the same street.
#'
#' @format A simple feature object with 4053 rows and 7 variables:
#' \describe{
#'   \item{pc4}{4-digit postal code}
#'   \item{areaname}{name of corresponding 4-digit postal code}
#'   \item{city}{name of city}
#'   \item{biggest_20cities}{pc4 is in one of the following twenty (biggest) cities in the Netherlands: Amsterdam, Rotterdam, 's-Gravenhage, Utrecht, Eindhoven,
#'   Tilburg, Groningen, Almere, Breda, Nijmegen, Enschede, Apeldoorn, Haarlem, Amersfoort, Arnhem, 's-Hertogenbosch, Zoetermeer, Zwolle, Maastricht, Leiden.}
#'   \item{geometry}{geometry object of postal code}
#'   \item{lon}{longitude of the 4-digit postal code centroid}
#'   \item{lat}{latitude of the 4-digit postal code centroid}
#' }
"nl_postcode4"

#' Object of class \code{sf} for countries of the entire world
#'
#' @description An object of class \code{sf} (simple feature) for countries of the entire world.
#'
#' @author Martin Haringa
#'
#' @format A simple feature object with 234 rows and 29 variables.
"world_countries"

#' Object of class \code{sf} for countries of Europe
#'
#' @description An object of class \code{sf} (simple feature) for countries of Europe
#'
#' @author Martin Haringa
#'
#' @details The epsg (SRID) is set to 102013 (Europe Albers Equal Area Conic).
#'
#' @format A simple feature object with 51 rows and 29 variables.
"europe_countries"

#' KNMI stations
#'
#' @description A data frame containing the IDs and meta-data on the official KNMI weather stations.
#'
#' @author Martin Haringa
#'
#' @format A data frame with 50 rows and 7 variables:
#' \describe{
#'   \item{station}{ID of the station (209-391)}
#'   \item{city}{City where the station is located}
#'   \item{lon}{Longitude of station (crs = 4326)}
#'   \item{lat}{Latitude of the station (crs = 4326)}
#'   \item{altitude}{Altitude of the station (in meters)}
#'   \item{X}{X coordinate of the station (crs = 32631)}
#'   \item{Y}{Y coordinate of the station (crs = 32631)}
#' }
"knmi_stations"

