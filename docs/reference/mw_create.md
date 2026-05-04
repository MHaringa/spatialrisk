# Create focal ("moving window") weight matrix

Create a focal ("moving window") weight matrix for use in
[`terra::focal()`](https://rspatial.github.io/terra/reference/focal.html).

## Usage

``` r
mw_create(r, radius)
```

## Arguments

- r:

  SpatRaster.

- radius:

  radius of the circle (in units of the crs).

## Details

`mw_create()` is a modified version of
[`terra::focalMat()`](https://rspatial.github.io/terra/reference/focalMat.html).
While
[`terra::focalMat()`](https://rspatial.github.io/terra/reference/focalMat.html)
creates a matrix where the border is the distance from the center of the
focal cell, `mw_create()` creates a matrix where the border of the
moving window is the distance from the edge of the focal cell.

## Author

Martin Haringa
