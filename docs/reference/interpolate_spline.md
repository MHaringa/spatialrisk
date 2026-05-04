# Interpolate values using spherical splines

Deprecated. Spline interpolation and smoothing on the sphere. This
function is outside the main scope of spatialrisk and will be removed in
a future release.

## Usage

``` r
interpolate_spline(
  observations,
  targets,
  value,
  lon_obs = lon,
  lat_obs = lat,
  lon_targets = lon,
  lat_targets = lat,
  k = 50
)
```

## Arguments

- observations:

  data.frame of observations.

- targets:

  data.frame of locations to calculate the interpolated and smoothed
  values for.

- value:

  Column with values in `observations`.

- lon_obs:

  Column in `observations` with longitude.

- lat_obs:

  Column in `observations` with latitude.

- lon_targets:

  Column in `targets` with longitude.

- lat_targets:

  Column in `targets` with latitude.

- k:

  Basis dimension. For small data sets reduce `k` manually.

## Value

Object equal to `targets` with an extra prediction column.

## References

[`Splines on the sphere`](https://rdrr.io/pkg/mgcv/man/smooth.construct.sos.smooth.spec.html)
