# Save highest concentrations per cell for subsequent iterations

Save highest concentrations per cell for subsequent iterations.

## Usage

``` r
update_db(hf_conc_new, db, cells)
```

## Arguments

- hf_conc_new:

  highest concentrations from the current iteration, obtained from
  [`conc_per_cell_new()`](https://mharinga.github.io/spatialrisk/reference/conc_per_cell_new.md).

- db:

  data.frame containing previously saved highest concentrations.

- cells:

  cells containing points associated with the current highest
  concentration to be removed from `db`.

## Author

Martin Haringa
