# Find the highest concentration for the current iteration

Find the highest concentration for the current iteration.

## Usage

``` r
highest_conc(hf_conc_new, high_foc, db)
```

## Arguments

- hf_conc_new:

  highest concentrations from the current iteration, retrieved from
  [`conc_per_cell_new()`](https://mharinga.github.io/spatialrisk/reference/conc_per_cell_new.md).

- high_foc:

  data.frame containing cell ids with the top n focal values from the
  current iteration.

- db:

  data.frame containing previously saved highest concentrations.

## Author

Martin Haringa
