# Package index

## Reference

`spatialrisk` is organised around fixed-radius aggregation, hotspot
detection, polygon-based spatial reporting, and supporting spatial data.
Most users can start with the core functions below, while lower-level
workflow functions are available for more advanced use.

## Core functions

Main entry points for common fixed-radius concentration and reporting
workflows.

- [`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md)
  : Find fixed-radius concentration hotspots
- [`radius_sum()`](https://mharinga.github.io/spatialrisk/reference/radius_sum.md)
  : Sum values within a radius around target coordinates
- [`points_within_radius()`](https://mharinga.github.io/spatialrisk/reference/points_within_radius.md)
  : Find points within radius around one or more centre coordinates
- [`summarise_points_by_polygon()`](https://mharinga.github.io/spatialrisk/reference/summarise_points_by_polygon.md)
  : Summarise point exposures by reporting polygon

## Fixed-radius aggregation

Evaluate point-level values and observations within a fixed radius.

- [`points_within_radius()`](https://mharinga.github.io/spatialrisk/reference/points_within_radius.md)
  : Find points within radius around one or more centre coordinates
- [`radius_sum()`](https://mharinga.github.io/spatialrisk/reference/radius_sum.md)
  : Sum values within a radius around target coordinates

## Concentration and hotspot detection

Find maximum fixed-radius concentrations and inspect the search
workflow.

### Main API

- [`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md)
  : Find fixed-radius concentration hotspots

### Advanced hotspot workflow

Lower-level functions underlying
[`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md)
for inspecting or running the search steps separately.

- [`prepare_spatialrisk()`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
  [`select_candidates()`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
  [`optimize_hotspot()`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
  [`plot(`*`<spatialrisk_hotspot_workflow>`*`)`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
  : Prepare fixed-radius concentration hotspot analysis
- [`plot(`*`<hotspot>`*`)`](https://mharinga.github.io/spatialrisk/reference/plot.hotspot.md)
  : Plot concentration hotspot results

## Polygon summaries and visualisation

Aggregate point exposures to reporting polygons and visualise spatial
values.

- [`summarise_points_by_polygon()`](https://mharinga.github.io/spatialrisk/reference/summarise_points_by_polygon.md)
  : Summarise point exposures by reporting polygon
- [`choropleth()`](https://mharinga.github.io/spatialrisk/reference/choropleth.md)
  : Create a choropleth map of polygon-level values
- [`map_points()`](https://mharinga.github.io/spatialrisk/reference/map_points.md)
  : Create interactive point map

## Data

Example portfolios, administrative boundaries, and weather data used in
package examples and workflows.

### Example datasets

- [`Groningen`](https://mharinga.github.io/spatialrisk/reference/Groningen.md)
  : Example addresses in Groningen
- [`insurance`](https://mharinga.github.io/spatialrisk/reference/insurance.md)
  : Example insurance portfolio

### Administrative and spatial boundaries

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

### Weather datasets

- [`knmi_historic_data()`](https://mharinga.github.io/spatialrisk/reference/knmi_historic_data.md)
  : Retrieve historic weather data for the Netherlands
- [`knmi_stations`](https://mharinga.github.io/spatialrisk/reference/knmi_stations.md)
  : KNMI weather stations

## Spatial utilities

General coordinate conversion, distance, and simple spatial display
helpers.

- [`haversine()`](https://mharinga.github.io/spatialrisk/reference/haversine.md)
  : Haversine great-circle distance
- [`convert_crs_df()`](https://mharinga.github.io/spatialrisk/reference/convert_crs_df.md)
  : Convert Coordinate Reference System (CRS)

## Deprecated

Deprecated functions retained for backward compatibility.

- [`concentration()`](https://mharinga.github.io/spatialrisk/reference/concentration.md)
  : Deprecated alias for radius_sum()
- [`points_in_circle()`](https://mharinga.github.io/spatialrisk/reference/points_in_circle.md)
  : Deprecated alias for points_within_radius()
- [`find_highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/find_highest_concentration.md)
  : Deprecated alias for concentration_hotspot()
- [`highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/highest_concentration.md)
  : Deprecated geohash hotspot search
- [`neighborhood_gh_search()`](https://mharinga.github.io/spatialrisk/reference/neighborhood_gh_search.md)
  : Deprecated geohash neighbourhood refinement
- [`plot_points()`](https://mharinga.github.io/spatialrisk/reference/plot_points.md)
  : Deprecated alias for map_points()
- [`points_to_polygon()`](https://mharinga.github.io/spatialrisk/reference/points_to_polygon.md)
  : Deprecated alias for summarise_points_by_polygon()
- [`choropleth_ggplot2()`](https://mharinga.github.io/spatialrisk/reference/choropleth_ggplot2.md)
  : Choropleth map of an sf object with ggplot2
- [`interpolate_spline()`](https://mharinga.github.io/spatialrisk/reference/interpolate_spline.md)
  : Interpolate values using spherical splines
- [`plot(`*`<conc>`*`)`](https://mharinga.github.io/spatialrisk/reference/plot.conc.md)
  : Plot deprecated geohash hotspot results
- [`plot(`*`<neighborhood>`*`)`](https://mharinga.github.io/spatialrisk/reference/plot.neighborhood.md)
  : Plot deprecated geohash neighbourhood results
