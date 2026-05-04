#' Retrieve historic weather data for the Netherlands
#'
#' This function retrieves historic hourly weather data collected by official
#' KNMI weather stations. See \code{\link{knmi_stations}} for the station
#' metadata included in this package.
#'
#' @param startyear,endyear Start and end year for the historic weather data.
#'   Both must be single whole years. KNMI hourly data is available from 1951.
#' @param stations Optional station IDs to download. The default,
#'   \code{NULL}, downloads all stations in \code{\link{knmi_stations}}.
#' @param progress Should a progress bar be shown? Defaults to
#'   \code{interactive()}.
#'
#' @return Data frame containing weather data and metadata for weather station
#' locations.
#'
#' @format The returned data frame contains the following columns:
#' \itemize{
#'   \item station = ID of measurement station;
#'   \item date = Date;
#'   \item FH = Hourly mean wind speed (in 0.1 m/s);
#'   \item FX = Maximum wind gust (in 0.1 m/s) during the hourly division;
#'   \item DR = Precipitation duration (in 0.1 hour) during the hourly division;
#'   \item RH = Hourly precipitation amount (in 0.1 mm) (-1 for <0.05 mm);
#'   \item city = City where the measurement station is located;
#'   \item lon = Longitude of station (EPSG:4326);
#'   \item lat = Latitude of station (EPSG:4326).
#' }
#'
#' @details The data is downloaded from KNMI when the function is called. This
#' requires an internet connection and may take some time when many stations or
#' years are requested.
#'
#' @importFrom dplyr left_join
#' @importFrom fs dir_create
#' @importFrom fs file_temp
#' @importFrom fs file_create
#' @importFrom fs path
#' @importFrom fs dir_ls
#' @importFrom fs file_size
#' @importFrom fs dir_delete
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom utils data
#' @importFrom utils download.file
#'
#' @author Martin Haringa
#'
#' @examples
#' \dontrun{
#' knmi_historic_data(2015, 2019, stations = c(260, 280))
#' }
#'
#' @export
knmi_historic_data <- function(startyear, endyear, stations = NULL,
                               progress = interactive()) {
  if (!requireNamespace("vroom", quietly = TRUE)) {
    stop("vroom is needed for this function. Install it with install.packages(\"vroom\").",
         call. = FALSE)
  }

  startyear <- validate_knmi_year(startyear, "startyear")
  endyear <- validate_knmi_year(endyear, "endyear")

  if (startyear < 1951) {
    stop("Historic hourly weather data before 1951 is not available.",
         call. = FALSE)
  }

  current_year <- as.POSIXlt(Sys.Date())$year + 1900
  if (endyear > current_year) {
    stop("`endyear` must not be greater than the current year.", call. = FALSE)
  }

  if (startyear > endyear) {
    stop("`startyear` must be smaller than or equal to `endyear`.",
         call. = FALSE)
  }

  if (!is.logical(progress) || length(progress) != 1L || is.na(progress)) {
    stop("`progress` must be `TRUE` or `FALSE`.", call. = FALSE)
  }

  utils::data("knmi_stations", envir = environment())
  id_stations <- validate_knmi_stations(stations, knmi_stations$station)

  historic_levels <- cut(
    seq.int(startyear, endyear),
    breaks = c(1951, seq(1960, 2200, by = 10)),
    labels = paste0(seq(1951, 2191, by = 10), "-",
                    seq(1960, 2200, by = 10)),
    include.lowest = TRUE,
    dig.lab = 5
  )
  periods <- unique(as.character(historic_levels))

  tmp <- fs::dir_create(fs::file_temp())
  on.exit(fs::dir_delete(tmp), add = TRUE)

  total_downloads <- length(id_stations) * length(periods)
  pb <- NULL
  if (progress) {
    pb <- utils::txtProgressBar(max = total_downloads, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  failed_urls <- character()
  download_index <- 0L

  for (station in id_stations) {
    for (period in periods) {
      download_index <- download_index + 1L
      if (progress) {
        utils::setTxtProgressBar(pb, download_index)
      }

      new_file <- fs::file_create(
        fs::path(tmp, paste0("knmi_", station, "_", period, ".zip"))
      )
      knmi_url <- paste0(
        "https://cdn.knmi.nl/knmi/map/page/",
        "klimatologie/gegevens/uurgegevens/uurgeg_",
        station, "_", period, ".zip"
      )

      downloaded <- tryCatch(
        {
          status <- utils::download.file(knmi_url, new_file, quiet = TRUE)
          identical(status, 0L)
        },
        error = function(e) FALSE,
        warning = function(w) FALSE
      )

      if (!downloaded) {
        failed_urls <- c(failed_urls, knmi_url)
      }
    }
  }

  if (length(failed_urls) > 0L) {
    warning(length(failed_urls), " KNMI download(s) failed.", call. = FALSE)
  }

  files <- fs::dir_ls(tmp, glob = "*zip")
  files_exist <- files[as.numeric(fs::file_size(files)) > 50 * 1024]

  if (length(files_exist) == 0L) {
    stop("No KNMI files were downloaded. Check the requested years, stations, and your internet connection.",
         call. = FALSE)
  }

  suppressMessages(
    df <- vroom::vroom(
      files_exist,
      skip = 31,
      delim = ",",
      col_select = list(station = 1, date = YYYYMMDD, HH, DD, FH, FF, FX, DR,
                        RH, Y),
      show_col_types = FALSE,
      progress = FALSE
    )[-1, ]
  )

  df$year <- as.numeric(substr(as.character(df$date), start = 1, stop = 4))
  df_selection <- subset(df, year >= startyear & year <= endyear)

  dplyr::left_join(
    df_selection,
    knmi_stations[, c("station", "city", "lon", "lat")],
    by = "station"
  )
}

validate_knmi_year <- function(year, name) {
  if (!is.numeric(year) || length(year) != 1L || is.na(year) ||
      !is.finite(year) || year != as.integer(year)) {
    stop("`", name, "` must be a single whole year.", call. = FALSE)
  }
  as.integer(year)
}

validate_knmi_stations <- function(stations, known_stations) {
  if (is.null(stations)) {
    return(known_stations)
  }

  if (!is.numeric(stations) || anyNA(stations) ||
      any(!is.finite(stations)) || any(stations != as.integer(stations))) {
    stop("`stations` must contain KNMI station IDs.", call. = FALSE)
  }

  stations <- unique(as.integer(stations))
  unknown <- setdiff(stations, known_stations)
  if (length(unknown) > 0L) {
    stop("Unknown KNMI station ID(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }

  stations
}
