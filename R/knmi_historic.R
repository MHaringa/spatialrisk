#' Retrieve historic weather data for the Netherlands
#'
#' This function retrieves historic weather data collected by the official KNMI weather stations. See spatialrisk::knmi_stations for a list of the official KNMI weather stations.
#'
#' @param startyear start year for historic weather data.
#' @param endyear end year for historic weather data.
#'
#' @return Data frame containing weather data and meta data for weather station locations.
#'
#' @format The returned data frame contains the following columns:
#' \itemize{
#'   \item station = ID of measurement station;
#'   \item date = Date;
#'   \item FH	= Hourly mean wind speed (in 0.1 m/s)
#'   \item FX	= Maximum wind gust (in 0.1 m/s) during the hourly division;
#'   \item T = Temperature (in 0.1 degrees Celsius) at 1.50 m at the time of observation;
#'   \item DR	= Precipitation duration (in 0.1 hour) during the hourly division;
#'   \item RH	= Hourly precipitation amount (in 0.1 mm) (-1 for <0.05 mm);
#'   \item city = City where the measurement station is located;
#'   \item lon = Longitude of station (crs = 4326);
#'   \item lat = Latitude of station (crs = 4326).
#' }
#'
#' @import dplyr
#' @import fs
#' @importFrom lubridate year
#' @importFrom lubridate today
#' @importFrom lubridate ymd
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom utils data
#' @importFrom utils download.file
#' @import vroom
#'
#' @author Martin Haringa
#'
#' @examples
#' \dontrun{
#' knmi_historic_data(2015, 2019)
#' }
#'
#' @export
knmi_historic_data <- function(startyear, endyear){

  # get reference data
  utils::data("knmi_stations", envir = environment())

  id_stations <- knmi_stations$station

  if ( startyear < 1951 ) { stop("Historic weather data before the year 1951 is not available.") }
  if( endyear > lubridate::year(lubridate::today()) ) { stop("Year end should not be greater than the current year.") }

  historic_levels <- cut(startyear:endyear,
                         breaks = c(1951, seq(1960, 2200, by = 10)),
                         labels = paste0(seq(1951, 2191, by = 10), "-", seq(1960, 2200, by = 10)),
                         include.lowest = TRUE, dig.lab = 5)

  periods <- unique(as.character(historic_levels))

  # Create a new directory
  tmp <- fs::dir_create(fs::file_temp())

  # Set progress bar
  pb <- utils::txtProgressBar(max = length(id_stations), style = 3)

  # create new files in the new directory
  for (i in 1:length(id_stations)){

    utils::setTxtProgressBar(pb, i)

    for (j in 1:length(periods)){
      new_file <- fs::file_create(fs::path(tmp, paste0("knmi_", id_stations[i], "_", periods[j], ".zip")))
      knmi_url <- paste0("https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/uurgegevens/uurgeg_", id_stations[i], "_", periods[j], ".zip")
      tryCatch(utils::download.file(knmi_url, new_file, quiet = TRUE),
               error = function(e) print(paste(knmi_url, 'is not found')))
    }
  }

  files <- fs::dir_ls(tmp, glob = "*zip")

  # Select existing files
  files_exist <- files[as.logical(fs::file_size(files) > "50KB")]

  # Read files into R
  suppressMessages(
    df <- vroom::vroom(files_exist, skip = 31, delim = ",",
                       col_select = list(station = 1, date = YYYYMMDD, HH, DD, FH, FF, FX, T, DR, RH, Y))[-1,]
  )

  # Delete directory
  fs::dir_delete(tmp)

  # Filter selected years
  df$year <- as.numeric(substr(as.character(df$date), start = 1, stop = 4))
  df_selection <- subset(df, year >= startyear & year <= endyear)

  # Add metadata
  df_meta <- dplyr::left_join(df_selection, knmi_stations[, c("station", "city", "lon", "lat")], by = "station")

  return(df_meta)
}


