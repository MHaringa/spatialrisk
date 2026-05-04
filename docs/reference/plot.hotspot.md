# Plot concentration hotspot results

Visualise objects returned by
[`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md).
The default plot shows hotspot centres, fixed-radius buffers, and the
contributing points. For terra-based results, diagnostic raster layers
can also be plotted.

## Usage

``` r
# S3 method for class 'hotspot'
plot(
  x,
  type = c("concentration", "focal", "rasterized", "updated_focal"),
  color1 = NULL,
  max.rad = 20,
  ...
)
```

## Arguments

- x:

  An object of class `hotspot`.

- type:

  Plot type. `"concentration"` shows hotspot buffers and contributing
  points and works for all hotspot search methods. `"focal"`,
  `"rasterized"`, and `"updated_focal"` are diagnostic terra layers and
  are only available for terra-based results.

- color1:

  Optional colour or colours for hotspot buffers and points. If `NULL`,
  colours are chosen with
  [`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html).

- max.rad:

  Maximum point radius passed to
  [`mapview::mapview()`](https://r-spatial.github.io/mapview/reference/mapView.html).
  Default is `20`.

- ...:

  Additional arguments passed to
  [`mapview::mapview()`](https://r-spatial.github.io/mapview/reference/mapView.html)
  for the contributing point layer when `type = "concentration"`, or to
  the raster mapview call for diagnostic raster layers.

## Value

A `mapview` object.

## Details

The observed-points hotspot method does not create terra raster or focal
objects. For observed-points results, use `type = "concentration"`.
