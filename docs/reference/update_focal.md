# Update current focal for the next iteration

Update current focal for the next iteration.

## Usage

``` r
update_focal(old_focal, new_rasterized, extent, mw)
```

## Arguments

- old_focal:

  Focal obtained from
  [`terra::focal()`](https://rspatial.github.io/terra/reference/focal.html)
  for the current iteration.

- new_rasterized:

  Output obtained from
  [`update_rasterize()`](https://mharinga.github.io/spatialrisk/reference/update_rasterize.md)
  for the next iteration.

- extent:

  Extent of the cells corresponding to the coordinates with the highest
  concentration for the current iteration. Extent is output from
  [`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html).

- mw:

  Moving window obtained from `create_mw()`.

## Details

Spatial extent refers to the geographic area covered by a spatial
dataset. It defines the boundaries in terms of its geographic
coordinates (north, east, south, west).

An focal is updated by the following steps:

1.  The extent of the cells corresponding to the coordinates with the
    highest concentration (xmin, xmax, ymin, ymax) is determined.

2.  A buffer of size two times the radius plus the cell size around this
    extent. All cells within this new extent will impact the focal
    values.

3.  Subset (crop) the rasterized data to include only the cells within
    this new extent. Perform focal calculations only for these new
    cells.

4.  As the focal values of the cells near the borders may be inflated,
    crop the result again to include only the cells within a radius of
    the original extent.

5.  Merge this updated focal with the previous focal object.

## Author

Martin Haringa
