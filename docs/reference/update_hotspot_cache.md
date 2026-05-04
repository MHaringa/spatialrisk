# Save highest concentrations per cell for subsequent iterations

Save highest concentrations per cell for subsequent iterations.

## Usage

``` r
update_hotspot_cache(new_candidates, cache, cells)
```

## Arguments

- new_candidates:

  highest concentrations from the current iteration.

- cache:

  data.frame containing previously saved highest concentrations.

- cells:

  cells containing points associated with the current highest
  concentration to be removed from `cache`.

## Author

Martin Haringa
