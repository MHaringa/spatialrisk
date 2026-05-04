# Determine the concentrations within the highest focal cells for the current iteration

Determine the concentrations within the highest focal cells for the
current iteration.

## Usage

``` r
conc_per_cell_new(
  high_foc,
  dff,
  value,
  size,
  points,
  db,
  radius,
  crs_from,
  crs_to,
  lon,
  lat
)
```

## Arguments

- high_foc:

  data.frame containing cell ids with the top n focal values from the
  current iteration.

- dff:

  data.frame with all observations

- value:

  column name in \`dff\` to find concentrations for.

- size:

  size of cell in meters.

- points:

  number of points per \`size\`.

- db:

  data.frame containing previously saved highest concentrations.

- radius:

  radius of circle in meters.

- crs_from:

  crs from

- crs_to:

  crs to

## Author

Martin Haringa
