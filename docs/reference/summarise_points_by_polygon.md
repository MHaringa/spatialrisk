# Summarise point exposures by reporting polygon

Spatially joins point data to polygon geometries and summarises a
numeric point exposure or value for each reporting area.

## Usage

``` r
summarise_points_by_polygon(
  polygons,
  points,
  value,
  fun = sum,
  lon = "lon",
  lat = "lat",
  crs = 4326,
  output_col = NULL,
  na.rm = TRUE,
  outside = c("message", "warning", "ignore"),
  repair_geometry = TRUE
)
```

## Arguments

- polygons:

  An object of class `sf` containing polygon geometries.

- points:

  A data.frame containing point coordinates and the value to summarise.

- value:

  A string giving the name of the numeric column in `points` to
  summarise.

- fun:

  A summary function, such as `sum`, `mean`, or `length`. Default is
  `sum`.

- lon:

  A string with the name of the longitude column in `points`. Default is
  `"lon"`.

- lat:

  A string with the name of the latitude column in `points`. Default is
  `"lat"`.

- crs:

  Coordinate reference system of the point coordinates. Default is
  `4326`.

- output_col:

  Optional string giving the name of the output column. If `NULL`, the
  name is created from `value` and `fun`, for example `"amount_sum"` or
  `"amount_mean"`.

- na.rm:

  Logical. Whether to remove missing values when `fun` supports an
  `na.rm` argument. Default is `TRUE`.

- outside:

  What to do when points fall outside all polygons: `"message"`
  (default), `"warning"`, or `"ignore"`.

- repair_geometry:

  Logical. Whether to try `sf::st_buffer(x, 0)` if transforming polygon
  geometries fails. Default is `TRUE`.

## Value

An `sf` object equal to `polygons` with an additional summary column.

## Examples

``` r
summarise_points_by_polygon(
  polygons = nl_postcode2,
  points = insurance,
  value = "amount",
  fun = sum
)
#> 80 points are outside any polygon.
#> Simple feature collection with 90 features and 4 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 3.358524 ymin: 50.7526 xmax: 7.226687 ymax: 53.55053
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    areaname      lon      lat amount_sum                       geometry
#> 1        10 4.879542 52.38225  129893079 POLYGON ((5.012625 52.34479...
#> 2        11 4.964842 52.32814   59755600 MULTIPOLYGON (((5.109895 52...
#> 3        12 5.155867 52.23687   48687601 MULTIPOLYGON (((5.317405 52...
#> 4        13 5.196764 52.35673   60964213 MULTIPOLYGON (((5.007205 52...
#> 5        14 4.942569 52.51892   45739347 MULTIPOLYGON (((4.826908 52...
#> 6        15 4.798242 52.46970   29442124 POLYGON ((4.863114 52.43076...
#> 7        16 5.087324 52.68566   29082847 MULTIPOLYGON (((5.029327 52...
#> 8        17 4.864428 52.85513   52473036 MULTIPOLYGON (((4.822374 53...
#> 9        18 4.740657 52.64247   37757398 POLYGON ((4.751882 52.59302...
#> 10       19 4.663460 52.52935   37393849 POLYGON ((4.682128 52.4188,...
```
