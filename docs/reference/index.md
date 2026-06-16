# Package index

## Fixed-radius aggregation

Core functions for selecting points and computing aggregated values
within a fixed radius.

- [`points_within_radius()`](https://mharinga.github.io/spatialrisk/reference/points_within_radius.md)
  : Find points within radius around one or more center coordinates
- [`radius_sum()`](https://mharinga.github.io/spatialrisk/reference/radius_sum.md)
  : Sum values within a radius around target coordinates
- [`haversine()`](https://mharinga.github.io/spatialrisk/reference/haversine.md)
  : Haversine great-circle distance

## Concentration and hotspot detection

Functions for identifying locations with high spatial concentration.

- [`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md)
  : Identify fixed-radius concentration hotspots
- [`prepare_spatialrisk()`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
  [`select_candidates()`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
  [`optimize_hotspot()`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
  [`plot(`*`<spatialrisk_hotspot_workflow>`*`)`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
  : Prepare fixed-radius concentration hotspot analysis
- [`plot(`*`<hotspot>`*`)`](https://mharinga.github.io/spatialrisk/reference/plot.hotspot.md)
  : Plot concentration hotspot results

## Visualisation

Functions for visualising spatial point data and aggregated spatial
values.

- [`map_points()`](https://mharinga.github.io/spatialrisk/reference/map_points.md)
  : Create interactive point map
- [`choropleth()`](https://mharinga.github.io/spatialrisk/reference/choropleth.md)
  : Create choropleth map
- [`summarise_points_by_polygon()`](https://mharinga.github.io/spatialrisk/reference/summarise_points_by_polygon.md)
  : Summarise point values by polygon

## Spatial data objects

Simple feature objects included with the package.

- [`nl_gemeente`](https://mharinga.github.io/spatialrisk/reference/nl_gemeente.md)
  : Municipalities in the Netherlands
- [`nl_provincie`](https://mharinga.github.io/spatialrisk/reference/nl_provincie.md)
  : Provinces in the Netherlands
- [`nl_corop`](https://mharinga.github.io/spatialrisk/reference/nl_corop.md)
  : COROP regions in the Netherlands
- [`nl_postcode2`](https://mharinga.github.io/spatialrisk/reference/nl_postcode2.md)
  : Two-digit postcode regions in the Netherlands
- [`nl_postcode3`](https://mharinga.github.io/spatialrisk/reference/nl_postcode3.md)
  : Three-digit postcode regions in the Netherlands
- [`nl_postcode4`](https://mharinga.github.io/spatialrisk/reference/nl_postcode4.md)
  : Four-digit postcode regions in the Netherlands

## Example data

Example datasets used in documentation and examples.

- [`Groningen`](https://mharinga.github.io/spatialrisk/reference/Groningen.md)
  : Example addresses in Groningen
- [`insurance`](https://mharinga.github.io/spatialrisk/reference/insurance.md)
  : Example insurance portfolio

## Weather data

- [`knmi_historic_data()`](https://mharinga.github.io/spatialrisk/reference/knmi_historic_data.md)
  : Retrieve historic weather data for the Netherlands
- [`knmi_stations`](https://mharinga.github.io/spatialrisk/reference/knmi_stations.md)
  : KNMI weather stations

## Utilities

- [`convert_crs_df()`](https://mharinga.github.io/spatialrisk/reference/convert_crs_df.md)
  : Convert Coordinate Reference System (CRS)

## Deprecated

- [`concentration()`](https://mharinga.github.io/spatialrisk/reference/concentration.md)
  : Deprecated alias for radius_sum()
- [`points_in_circle()`](https://mharinga.github.io/spatialrisk/reference/points_in_circle.md)
  : Deprecated alias for points_within_radius()
- [`find_highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/find_highest_concentration.md)
  : Deprecated alias for concentration_hotspot()
- [`highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/highest_concentration.md)
  : Highest concentration risk
- [`neighborhood_gh_search()`](https://mharinga.github.io/spatialrisk/reference/neighborhood_gh_search.md)
  : Search for coordinates with higher concentrations within geohash
- [`plot_points()`](https://mharinga.github.io/spatialrisk/reference/plot_points.md)
  : Deprecated alias for map_points()
- [`points_to_polygon()`](https://mharinga.github.io/spatialrisk/reference/points_to_polygon.md)
  : Deprecated alias for summarise_points_by_polygon()
- [`choropleth_ggplot2()`](https://mharinga.github.io/spatialrisk/reference/choropleth_ggplot2.md)
  : Choropleth map of an sf object with ggplot2
- [`interpolate_spline()`](https://mharinga.github.io/spatialrisk/reference/interpolate_spline.md)
  : Interpolate values using spherical splines
- [`plot(`*`<conc>`*`)`](https://mharinga.github.io/spatialrisk/reference/plot.conc.md)
  : Automatically create a plot for objects obtained from
  highest_concentration()
- [`plot(`*`<neighborhood>`*`)`](https://mharinga.github.io/spatialrisk/reference/plot.md)
  : Automatically create a plot for objects obtained from
  neighborhood_gh_search()
