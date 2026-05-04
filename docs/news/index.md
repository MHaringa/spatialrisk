# Changelog

## spatialrisk 0.8.0

### Main API changes

- Added
  [`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md)
  as the main function for fixed-radius concentration hotspot detection.
  The function returns a `hotspot` object with `hotspots` and
  `contributing_points` components.
- Added `method = "continuous"` as the default method for
  [`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md).
  It searches for hotspot centres that may lie between observed points,
  using spatial screening followed by Rcpp pair-intersection refinement.
  If the local subset exceeds `max_refinement_points`, it falls back to
  `method = "grid"`.
- Added `method = "observed"` to
  [`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md),
  an experimental Rcpp spatial-indexing alternative that searches
  observed point locations only.
- Deprecated
  [`find_highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/spatialrisk-deprecated.md)
  in favour of
  [`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md).
  The deprecated function remains available for existing code.
- Renamed the
  [`concentration()`](https://mharinga.github.io/spatialrisk/reference/radius_sum.md)
  workflow to
  [`radius_sum()`](https://mharinga.github.io/spatialrisk/reference/radius_sum.md),
  with clearer `targets` and `reference` arguments. The old
  [`concentration()`](https://mharinga.github.io/spatialrisk/reference/radius_sum.md)
  function remains available as a deprecated compatibility wrapper.
- Renamed `radius_sum(display_progress = )` to `radius_sum(progress = )`
  for consistency with
  [`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md).
- Renamed
  [`points_in_circle()`](https://mharinga.github.io/spatialrisk/reference/points_within_radius.md)
  to
  [`points_within_radius()`](https://mharinga.github.io/spatialrisk/reference/points_within_radius.md).
  The old function remains available as a deprecated compatibility
  wrapper.
- Replaced
  [`points_to_polygon()`](https://mharinga.github.io/spatialrisk/reference/summarise_points_by_polygon.md)
  with
  [`summarise_points_by_polygon()`](https://mharinga.github.io/spatialrisk/reference/summarise_points_by_polygon.md),
  which more clearly describes joining point data to polygons and
  summarising a numeric point attribute. The old function remains
  available as a deprecated compatibility wrapper.
- Renamed
  [`plot_points()`](https://mharinga.github.io/spatialrisk/reference/map_points.md)
  to
  [`map_points()`](https://mharinga.github.io/spatialrisk/reference/map_points.md).
  The old function remains available as a deprecated compatibility
  wrapper.

### Function improvements

- Improved
  [`haversine()`](https://mharinga.github.io/spatialrisk/reference/haversine.md)
  and the underlying C++ distance calculation by reducing unnecessary
  operations and adding stricter input checks.
- Improved
  [`points_within_radius()`](https://mharinga.github.io/spatialrisk/reference/points_within_radius.md)
  validation and handling of multiple centre coordinates.
- Improved
  [`radius_sum()`](https://mharinga.github.io/spatialrisk/reference/radius_sum.md)
  validation, output column handling, and C++ prefiltering of incomplete
  reference rows.
- Updated
  [`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md)
  documentation to clarify that the function uses a grid-based search
  with local refinement. The search precision is controlled by
  `cell_size` and `grid_precision`.
- [`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md)
  now uses `progress` instead of `print_progress`. Since this is a new
  public API, the old argument is not retained there; older deprecated
  functions still translate their legacy progress arguments.
- Deprecated
  [`interpolate_spline()`](https://mharinga.github.io/spatialrisk/reference/interpolate_spline.md)
  because spline interpolation is outside the main scope of the package.
  The function remains available for compatibility.

### Mapping and visualisation

- Consolidated choropleth mapping around
  [`choropleth()`](https://mharinga.github.io/spatialrisk/reference/choropleth.md),
  using `tmap` for both static and interactive output.
- Deprecated
  [`choropleth_ggplot2()`](https://mharinga.github.io/spatialrisk/reference/choropleth_ggplot2.md)
  in favour of
  [`choropleth()`](https://mharinga.github.io/spatialrisk/reference/choropleth.md).
- Moved optional visualisation packages such as `mapview`, `tmap`,
  `ggplot2`, and `classInt` to `Suggests`.

### Data and documentation

- Reorganised dataset documentation by topic: Dutch spatial boundary
  data, KNMI stations, insurance example data, and Groningen example
  addresses.
- Moved
  [`knmi_historic_data()`](https://mharinga.github.io/spatialrisk/reference/knmi_historic_data.md)
  to its own function documentation file and improved validation for
  years, station IDs, progress handling, failed downloads, and
  temporary-file cleanup.
- Added tests for package dataset schemas and
  [`knmi_historic_data()`](https://mharinga.github.io/spatialrisk/reference/knmi_historic_data.md)
  input validation.
- Added a main vignette on fixed-radius concentration analysis in an
  applied insurance setting.
- Updated the visualisation vignette to use the current
  [`choropleth()`](https://mharinga.github.io/spatialrisk/reference/choropleth.md)
  API.
- Rewrote the README in a more applied, method-oriented style suitable
  for use alongside an actuarial paper.
- Updated `DESCRIPTION` with a more specific title and broader package
  description.
- Centralised small deprecated compatibility wrappers in one file.

## spatialrisk 0.7.3

CRAN release: 2025-09-14

- Updated and improved all function documentation (roxygen2).

## spatialrisk 0.7.2

CRAN release: 2025-05-25

- Introduced
  [`find_highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/spatialrisk-deprecated.md):
  a faster and more accurate alternative to
  [`highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/highest_concentration.md),
  leveraging focal statistics for optimal results.
- Deprecated
  [`highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/highest_concentration.md)
  in favor of the new, improved function.
- Updated
  [`plot_points()`](https://mharinga.github.io/spatialrisk/reference/map_points.md)
  to utilize
  [`mapview::mapview()`](https://r-spatial.github.io/mapview/reference/mapView.html)
  for enhanced interactive map visualizations.
- Revised the README to reflect these new features and updates.

## spatialrisk 0.7.1

CRAN release: 2024-02-21

- [`plot_points()`](https://mharinga.github.io/spatialrisk/reference/map_points.md)
  now returns a warning when a specific provider tile is not available.

## spatialrisk 0.7.0

CRAN release: 2021-11-10

- [`neighborhood_gh_search()`](https://mharinga.github.io/spatialrisk/reference/neighborhood_gh_search.md)
  now returns a more precise outcome when the radius of the circle is
  not equal to 200m.
- [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  is used for `sf` objects to not show the message that old crs is
  detected anymore.

## spatialrisk 0.6.9

CRAN release: 2021-05-26

- [`highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/highest_concentration.md)
  now returns correct highest concentration when the circle of the
  highest concentration overlaps more than one geohash.
- `plot.concentration()` now handles many lon/lat pairs better.

## spatialrisk 0.6.8

CRAN release: 2021-05-18

- [`highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/highest_concentration.md)
  is added to do a fast search for the coordinates of the highest
  concentration.
- [`neighborhood_gh_search()`](https://mharinga.github.io/spatialrisk/reference/neighborhood_gh_search.md)
  is added to look for even higher concentrations in the neighborhood of
  the coordinates found by
  [`highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/highest_concentration.md).

## spatialrisk 0.6.7

CRAN release: 2020-04-23

- A package website is added using pkgdown.
- [`concentration()`](https://mharinga.github.io/spatialrisk/reference/radius_sum.md)
  and
  [`points_in_circle()`](https://mharinga.github.io/spatialrisk/reference/points_within_radius.md)
  now return error messages when the data does not contain columns for
  `lon` and `lat`.
- [`concentration()`](https://mharinga.github.io/spatialrisk/reference/radius_sum.md)
  and
  [`points_in_circle()`](https://mharinga.github.io/spatialrisk/reference/points_within_radius.md)
  have updated documentation.
- [`haversine()`](https://mharinga.github.io/spatialrisk/reference/haversine.md)
  now returns NA when coordinates are missing.

## spatialrisk 0.6.6

CRAN release: 2020-03-21

- [`points_to_polygon()`](https://mharinga.github.io/spatialrisk/reference/summarise_points_by_polygon.md)
  is updated to keep spatialrisk and a new version of dplyr working
  together smoothly.

## spatialrisk 0.6.5

CRAN release: 2019-11-06

- [`knmi_historic_data()`](https://mharinga.github.io/spatialrisk/reference/knmi_historic_data.md)
  and
  [`knmi_stations()`](https://mharinga.github.io/spatialrisk/reference/knmi_stations.md)
  are added to retrieve data from weather stations in the Netherlands.

## spatialrisk 0.6.4

CRAN release: 2019-10-25

- `interpolate_krige()` and
  [`interpolate_spline()`](https://mharinga.github.io/spatialrisk/reference/interpolate_spline.md)
  are added for interpolation and smoothing on the sphere.

## spatialrisk 0.6.3

CRAN release: 2019-10-18

- Data sets
  [`nl_corop()`](https://mharinga.github.io/spatialrisk/reference/nl_corop.md),
  [`nl_gemeente()`](https://mharinga.github.io/spatialrisk/reference/nl_gemeente.md),
  `nl_postcode1()`,
  [`nl_postcode2()`](https://mharinga.github.io/spatialrisk/reference/nl_postcode2.md),
  [`nl_postcode3()`](https://mharinga.github.io/spatialrisk/reference/nl_postcode3.md),
  [`nl_postcode4()`](https://mharinga.github.io/spatialrisk/reference/nl_postcode4.md),
  and
  [`nl_provincie()`](https://mharinga.github.io/spatialrisk/reference/nl_provincie.md)
  now contain columns `longitude` and `latitude` for the centroid of the
  polygons.

## spatialrisk 0.6.2

CRAN release: 2019-07-29

- Non UTF-8 character removed from column `areaname` in data set
  [`nl_gemeente()`](https://mharinga.github.io/spatialrisk/reference/nl_gemeente.md).
- In
  [`choropleth()`](https://mharinga.github.io/spatialrisk/reference/choropleth.md),
  `n` specifies the number of clusters in a map.
- In data sets
  [`nl_corop()`](https://mharinga.github.io/spatialrisk/reference/nl_corop.md),
  [`nl_gemeente()`](https://mharinga.github.io/spatialrisk/reference/nl_gemeente.md),
  `nl_postcode1()`,
  [`nl_postcode2()`](https://mharinga.github.io/spatialrisk/reference/nl_postcode2.md),
  [`nl_postcode3()`](https://mharinga.github.io/spatialrisk/reference/nl_postcode3.md),
  [`nl_postcode4()`](https://mharinga.github.io/spatialrisk/reference/nl_postcode4.md),
  and `nl_provincie`, column `areaname` now refers to the region.

## spatialrisk 0.6.1

CRAN release: 2019-04-30

- In
  [`points_to_polygon()`](https://mharinga.github.io/spatialrisk/reference/summarise_points_by_polygon.md),
  `outside_print` shows the points that are not within any polygon.

## spatialrisk 0.6.0

CRAN release: 2019-04-08

- [`points_to_polygon()`](https://mharinga.github.io/spatialrisk/reference/summarise_points_by_polygon.md)
  is added.
- Data sets `europe_countries()` and `world_countries()` are added.

## spatialrisk 0.5.1

CRAN release: 2019-03-01

- In
  [`concentration()`](https://mharinga.github.io/spatialrisk/reference/radius_sum.md)
  the algorithm for testing if a point is inside a circle is updated by
  testing whether the point is inside a square diamond inside the
  square.

## spatialrisk 0.3.1

CRAN release: 2018-09-12

- `NEWS.md` is added to track changes to the package.
