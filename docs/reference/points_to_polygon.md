# Map points to polygons

Join a data.frame containing coordinates (longitude and latitude) to
polygon geometries. Arithmetic operations are then applied to the
attributes of the joined coordinates to obtain aggregated values for
each polygon.

## Usage

``` r
points_to_polygon(sf_map, df, oper, crs = 4326, outside_print = FALSE)
```

## Arguments

- sf_map:

  object of class sf representing the polygon geometries.

- df:

  data.frame containing coordinates (column names should be 'lon' and
  'lat')

- oper:

  arithmetic operation to be applied on the polygon level.

- crs:

  coordinate reference system (default is 4326).

- outside_print:

  logical indicating whether to print points that are not within a
  polygon (default is FALSE).

## Value

An object of class `sf`

## Author

Martin Haringa

## Examples

``` r
points_to_polygon(nl_postcode2, insurance, sum(amount, na.rm = TRUE))
#> 80 points are outside any polygon.
#> Simple feature collection with 90 features and 5 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 3.358524 ymin: 50.7526 xmax: 7.226687 ymax: 53.55053
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    id areaname      lon      lat    output                       geometry
#> 1   1       10 4.879542 52.38225 129893079 POLYGON ((5.012625 52.34479...
#> 2   2       11 4.964842 52.32814  59755600 MULTIPOLYGON (((5.109895 52...
#> 3   3       12 5.155867 52.23687  48687601 MULTIPOLYGON (((5.317405 52...
#> 4   4       13 5.196764 52.35673  60964213 MULTIPOLYGON (((5.007205 52...
#> 5   5       14 4.942569 52.51892  45739347 MULTIPOLYGON (((4.826908 52...
#> 6   6       15 4.798242 52.46970  29442124 POLYGON ((4.863114 52.43076...
#> 7   7       16 5.087324 52.68566  29082847 MULTIPOLYGON (((5.029327 52...
#> 8   8       17 4.864428 52.85513  52473036 MULTIPOLYGON (((4.822374 53...
#> 9   9       18 4.740657 52.64247  37757398 POLYGON ((4.751882 52.59302...
#> 10 10       19 4.663460 52.52935  37393849 POLYGON ((4.682128 52.4188,...
if (FALSE) { # \dontrun{
shp_read <- sf::st_read("~/path/to/file.shp")
points_to_polygon(shp_read, insurance, sum(amount, na.rm = TRUE))
} # }
```
