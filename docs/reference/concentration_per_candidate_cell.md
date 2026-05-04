# Determine the concentrations within the highest focal cells for the current iteration

Determine the concentrations within the highest focal cells for the
current iteration.

## Usage

``` r
concentration_per_candidate_cell(
  candidate_cells,
  dff,
  value,
  size,
  points,
  cache,
  radius,
  crs_metric,
  lon,
  lat
)
```

## Arguments

- candidate_cells:

  data.frame containing cell ids with focal values selected for
  refinement in the current iteration.

- dff:

  data.frame with all observations

- value:

  column name in \`dff\` to find concentrations for.

- size:

  size of cell in meters.

- points:

  number of points per \`size\`.

- cache:

  data.frame containing previously saved highest concentrations.

- radius:

  radius of circle in meters.

- crs_metric:

  metric CRS used for candidate cell coordinates.

- lon:

  longitude column name.

- lat:

  latitude column name.

## Author

Martin Haringa
