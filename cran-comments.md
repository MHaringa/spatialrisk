## Resubmission
This is a resubmission. In this version I have:

### Main API changes

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

### Mapping and visualisation

* Consolidated choropleth mapping around `choropleth()`, using `tmap` for both
  static and interactive output.
* Deprecated `choropleth_ggplot2()` in favour of `choropleth()`.
* Moved optional visualisation packages such as `mapview`, `tmap`, `ggplot2`,
  and `classInt` to `Suggests`.

### Data and documentation

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

## Test environments
* local OS X install, R 4.5.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
I have also run R CMD check on downstream dependencies of spatialrisk.
All packages that I could install passed.


