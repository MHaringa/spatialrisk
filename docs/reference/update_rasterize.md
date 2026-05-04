# Update current rasterize for the next iteration

Update current rasterize for the next iteration.

## Usage

``` r
update_rasterize(old_rasterized, extent, new_spatvector, col)
```

## Arguments

- old_rasterized:

  SpatRaster used in the current iteration.

- extent:

  Extent of the cells corresponding to the coordinates with the highest
  concentration for the current iteration.Extent is output from
  terra::ext().

- new_spatvector:

  Updated SpatVector for next iteration.

- col:

  Character. Variable name in `new_spatvector`.

## Details

Spatial extent refers to the geographic area covered by a spatial
dataset. It defines the boundaries in terms of its geographic
coordinates (north, east, south, west).

## Author

Martin Haringa
