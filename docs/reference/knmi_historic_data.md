# Retrieve historic weather data for the Netherlands

This function retrieves historic hourly weather data collected by
official KNMI weather stations. See
[`knmi_stations`](https://mharinga.github.io/spatialrisk/reference/knmi_stations.md)
for the station metadata included in this package.

## Usage

``` r
knmi_historic_data(
  startyear,
  endyear,
  stations = NULL,
  progress = interactive()
)
```

## Format

The returned data frame contains the following columns:

- station = ID of measurement station;

- date = Date;

- FH = Hourly mean wind speed (in 0.1 m/s);

- FX = Maximum wind gust (in 0.1 m/s) during the hourly division;

- DR = Precipitation duration (in 0.1 hour) during the hourly division;

- RH = Hourly precipitation amount (in 0.1 mm) (-1 for \<0.05 mm);

- city = City where the measurement station is located;

- lon = Longitude of station (EPSG:4326);

- lat = Latitude of station (EPSG:4326).

## Arguments

- startyear, endyear:

  Start and end year for the historic weather data. Both must be single
  whole years. KNMI hourly data is available from 1951.

- stations:

  Optional station IDs to download. The default, `NULL`, downloads all
  stations in
  [`knmi_stations`](https://mharinga.github.io/spatialrisk/reference/knmi_stations.md).

- progress:

  Should a progress bar be shown? Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

Data frame containing weather data and metadata for weather station
locations.

## Details

The data is downloaded from KNMI when the function is called. This
requires an internet connection and may take some time when many
stations or years are requested.

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
knmi_historic_data(2015, 2019, stations = c(260, 280))
} # }
```
