# Search for coordinates with higher concentrations within geohash

[`highest_concentration`](https://mharinga.github.io/spatialrisk/reference/highest_concentration.md)
returns the highest concentration within a portfolio based on grid
points. However, higher concentrations can be found within two grid
points. \`neighborhood_gh_search()\` looks for even higher
concentrations in the neighborhood of the grid points with the highest
concentrations. This optimization is done by means of Simulated
Annealing.

## Usage

``` r
neighborhood_gh_search(
  hc,
  highest_geohash = 1,
  max.call = 1000,
  verbose = TRUE,
  seed = 1
)
```

## Arguments

- hc:

  object of class \`concentration\` obtained from
  \`highest_concentration()\`

- highest_geohash:

  the number of geohashes the searching algorithm is applied to.
  Defaults to 1 (i.e. algorithm is only applied to the geohash with the
  highest concentration).

- max.call:

  maximum number of calls to the concentration function (i.e. the number
  of coordinates in the neighborhood of the highest concentration).
  Defaults to 1000.

- verbose:

  show messages from the algorithm (TRUE/FALSE). Defaults to FALSE.

- seed:

  set seed

## Value

data.frame

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
# Find highest concentration with a precision of a grid of 25 meters
hc1 <- highest_concentration(Groningen, amount, radius = 200,
 grid_distance = 25)

# Increase the number of calls for more extensive search
hc1_nghb <- neighborhood_gh_search(hc1, max.call = 7000, highest_geohash = 1)
hc2_nghb <- neighborhood_gh_search(hc1, max.call = 7000, highest_geohash = 2)
plot(hc1_nghb)
plot(hc2_nghb)
} # }
```
