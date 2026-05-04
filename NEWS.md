# spatialrisk 0.8.0

## Main API changes

* Added `concentration_hotspot()` as the main function for fixed-radius
  concentration hotspot detection. The function returns a `hotspot` object with
  `hotspots` and `contributing_points` components.
* Added `method = "continuous"` as the default method for
  `concentration_hotspot()`. It searches for hotspot centres that may lie
  between observed points, using spatial screening followed by Rcpp
  pair-intersection refinement. If the local subset exceeds
  `max_refinement_points`, it falls back to `method = "grid"`.
* Added `method = "observed"` to `concentration_hotspot()`, an experimental
  Rcpp spatial-indexing alternative that searches observed point locations
  only.
* Deprecated `find_highest_concentration()` in favour of
  `concentration_hotspot()`. The deprecated function remains available for
  existing code.
* Renamed the `concentration()` workflow to `radius_sum()`, with clearer
  `targets` and `reference` arguments. The old `concentration()` function
  remains available as a deprecated compatibility wrapper.
* Renamed `radius_sum(display_progress = )` to `radius_sum(progress = )` for
  consistency with `concentration_hotspot()`.
* Renamed `points_in_circle()` to `points_within_radius()`. The old function
  remains available as a deprecated compatibility wrapper.
* Replaced `points_to_polygon()` with `summarise_points_by_polygon()`, which
  more clearly describes joining point data to polygons and summarising a
  numeric point attribute. The old function remains available as a deprecated
  compatibility wrapper.
* Renamed `plot_points()` to `map_points()`. The old function remains available
  as a deprecated compatibility wrapper.

## Function improvements

* Improved `haversine()` and the underlying C++ distance calculation by reducing
  unnecessary operations and adding stricter input checks.
* Improved `points_within_radius()` validation and handling of multiple centre
  coordinates.
* Improved `radius_sum()` validation, output column handling, and C++ prefiltering
  of incomplete reference rows.
* Updated `concentration_hotspot()` documentation to clarify that the function
  uses a grid-based search with local refinement. The search precision is
  controlled by `cell_size` and `grid_precision`.
* `concentration_hotspot()` now uses `progress` instead of `print_progress`.
  Since this is a new public API, the old argument is not retained there; older
  deprecated functions still translate their legacy progress arguments.
* Deprecated `interpolate_spline()` because spline interpolation is outside the
  main scope of the package. The function remains available for compatibility.

## Mapping and visualisation

* Consolidated choropleth mapping around `choropleth()`, using `tmap` for both
  static and interactive output.
* Deprecated `choropleth_ggplot2()` in favour of `choropleth()`.
* Moved optional visualisation packages such as `mapview`, `tmap`, `ggplot2`,
  and `classInt` to `Suggests`.

## Data and documentation

* Reorganised dataset documentation by topic: Dutch spatial boundary data,
  KNMI stations, insurance example data, and Groningen example addresses.
* Moved `knmi_historic_data()` to its own function documentation file and
  improved validation for years, station IDs, progress handling, failed
  downloads, and temporary-file cleanup.
* Added tests for package dataset schemas and `knmi_historic_data()` input
  validation.
* Added a main vignette on fixed-radius concentration analysis in an applied
  insurance setting.
* Updated the visualisation vignette to use the current `choropleth()` API.
* Rewrote the README in a more applied, method-oriented style suitable for use
  alongside an actuarial paper.
* Updated `DESCRIPTION` with a more specific title and broader package
  description.
* Centralised small deprecated compatibility wrappers in one file.

# spatialrisk 0.7.3

* Updated and improved all function documentation (roxygen2).

# spatialrisk 0.7.2

* Introduced `find_highest_concentration()`: a faster and more accurate alternative to `highest_concentration()`, leveraging focal statistics for optimal results.
* Deprecated `highest_concentration()` in favor of the new, improved function.
* Updated `plot_points()` to utilize `mapview::mapview()` for enhanced interactive map visualizations.
* Revised the README to reflect these new features and updates.

# spatialrisk 0.7.1

* `plot_points()` now returns a warning when a specific provider tile is not available.

# spatialrisk 0.7.0

* `neighborhood_gh_search()` now returns a more precise outcome when the radius of the circle is not equal to 200m.
* `sf::st_crs()` is used for `sf` objects to not show the message that old crs is detected anymore.

# spatialrisk 0.6.9

* `highest_concentration()` now returns correct highest concentration when the circle of the highest concentration overlaps more than one geohash.
* `plot.concentration()` now handles many lon/lat pairs better.

# spatialrisk 0.6.8

* `highest_concentration()` is added to do a fast search for the coordinates of the highest concentration.
* `neighborhood_gh_search()` is added to look for even higher concentrations in the neighborhood of the coordinates found by `highest_concentration()`.

# spatialrisk 0.6.7

* A package website is added using pkgdown.
* `concentration()` and `points_in_circle()` now return error messages when the data does not contain columns for `lon` and `lat`.
* `concentration()` and `points_in_circle()` have updated documentation.
* `haversine()` now returns NA when coordinates are missing.

# spatialrisk 0.6.6

* `points_to_polygon()` is updated to keep spatialrisk and a new version of dplyr working together smoothly.

# spatialrisk 0.6.5

* `knmi_historic_data()` and `knmi_stations()` are added to retrieve data from weather stations in the Netherlands.

# spatialrisk 0.6.4

* `interpolate_krige()` and `interpolate_spline()` are added for interpolation and smoothing on the sphere. 

# spatialrisk 0.6.3

* Data sets `nl_corop()`, `nl_gemeente()`, `nl_postcode1()`, `nl_postcode2()`, `nl_postcode3()`, `nl_postcode4()`, and `nl_provincie()` now contain columns `longitude` and `latitude` for the centroid of the polygons. 

# spatialrisk 0.6.2

* Non UTF-8 character removed from column `areaname` in data set `nl_gemeente()`.
* In `choropleth()`, `n` specifies the number of clusters in a map.
* In data sets `nl_corop()`, `nl_gemeente()`, `nl_postcode1()`, `nl_postcode2()`, `nl_postcode3()`, `nl_postcode4()`, and `nl_provincie`, column `areaname` now refers to the region. 

# spatialrisk 0.6.1

* In `points_to_polygon()`, `outside_print` shows the points that are not within any polygon. 

# spatialrisk 0.6.0

* `points_to_polygon()` is added.
* Data sets `europe_countries()` and `world_countries()` are added.

# spatialrisk 0.5.1

* In `concentration()` the algorithm for testing if a point is inside a circle is updated by testing whether the point is inside a square diamond inside the square. 

# spatialrisk 0.3.1

* `NEWS.md` is added to track changes to the package.
