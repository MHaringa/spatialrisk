## Submission

This is version 0.8.4 of spatialrisk, a maintenance release following 0.8.3.

The main changes are:

* Interactive maps now use `Esri.WorldGrayCanvas` by default, with
  `OpenStreetMap` and `Esri.WorldImagery` as alternatives. CARTO basemaps have
  been removed from the default choices because they now require an API key,
  as reported in [mapview issue #520](https://github.com/r-spatial/mapview/issues/520).
* A `basemaps` argument allows users to choose background maps in
  `map_points()`, hotspot and workflow plots, and `choropleth()`. The legacy
  Leaflet plot methods retain their existing `providers` argument.
* Interactive `choropleth()` examples are wrapped in `\dontrun{}` to avoid
  opening interactive maps when running package examples.

## R CMD check results

To be completed with the check results for version 0.8.4 before submission.
