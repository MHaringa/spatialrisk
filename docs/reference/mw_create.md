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

The returned mask includes raster-cell centres whose Euclidean distance
from the focal-cell centre is no greater than \`radius\` plus the full
raster-cell diagonal. This conservative expansion accounts for the
possible displacement of both a disk centre and a contributing point
from their respective cell centres.

## Author

Martin Haringa
