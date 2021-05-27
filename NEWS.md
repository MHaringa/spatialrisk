# spatialrisk 0.7.0


# spatialrisk 0.6.9

* `highest_concentration()` now returns correct highest concentration when the circle of the highest concentration overlaps more than one geohash
* `plot.concentration()` now handles many lon/lat pairs better

# spatialrisk 0.6.8

* `highest_concentration()` is added to do a fast search for the coordinates of the highest concentration
* `neighborhood_gh_search()` is added to look for even higher concentrations in the neighborhood of the coordinates found by `highest_concentration()`

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
