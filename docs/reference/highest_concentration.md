# Highest concentration risk

Find the centre coordinates of a circle with a fixed radius that
maximizes the coverage of total fire risk insured.
\`highest_concentration()\` returns the coordinates (lon/lat) and the
corresponding concentration. The concentration is defined as the sum of
all observations within a circle of a certain radius. See
[`concentration`](https://mharinga.github.io/spatialrisk/reference/concentration.md)
for determining concentration for pre-defined coordinates.

## Usage

``` r
highest_concentration(
  df,
  value,
  lon = lon,
  lat = lat,
  lowerbound = NULL,
  radius = 200,
  grid_distance = 25,
  gh_precision = 6,
  display_progress = TRUE
)
```

## Arguments

- df:

  data.frame of locations, should at least include column for longitude,
  latitude and sum insured.

- value:

  column name with value of interest to summarize (e.g. sum insured).

- lon:

  column name with longitude (defaults to \`lon\`).

- lat:

  column name with latitude (defaults to \`lat\`).

- lowerbound:

  set lowerbound.

- radius:

  radius (in meters) (default is 200m).

- grid_distance:

  distance (in meters) for precision of concentration risk (default is
  25m). \`neighborhood_search()\` can be used to search for coordinates
  with even higher concentrations in the neighborhood of the highest
  concentrations.

- gh_precision:

  set precision for geohash.

- display_progress:

  show progress bar (TRUE/FALSE). Defaults to TRUE.

## Value

data.frame with coordinates (lon/lat) with the highest concentrations

## Details

A recently European Commission regulation requires insurance companies
to determine the maximum value of insured fire risk policies of all
buildings that are partly or fully located within circle of a radius of
200m (Commission Delegated Regulation (EU), 2015, Article 132). The
problem can be stated as: "find the centre coordinates of a circle with
a fixed radius that maximizes the coverage of total fire risk insured".
This can be viewed as a particular instance of the Maximal Covering
Location Problem (MCLP) with fixed radius. See Gomes (2018) for a
solution to the maximum fire risk insured capital problem using a
multi-start local search meta-heuristic. The computational performance
of `highest_concentration()` is investigated to overcome the long times
the MCLP algorithm is taking. `highest_concentration()` is written in
C++, and for 500,000 buildings it needs about 5-10 seconds to determine
the maximum value of insured fire risk policies that are partly or fully
located within circle of a radius of 200m.

## References

Commission Delegated Regulation (EU) (2015). Solvency II Delegated Act
2015/35. Official Journal of the European Union, 58:124.

Gomes M.I., Afonso L.B., Chibeles-Martins N., Fradinho J.M. (2018).
Multi-start Local Search Procedure for the Maximum Fire Risk Insured
Capital Problem. In: Lee J., Rinaldi G., Mahjoub A. (eds) Combinatorial
Optimization. ISCO 2018. Lecture Notes in Computer Science, vol 10856.
Springer, Cham. \<doi:10.1007/978-3-319-96151-4_19\>

## Author

Martin Haringa

## Examples

``` r
 if (FALSE) { # \dontrun{
# Find highest concentration with a precision of a grid of 25 meters
hc1 <- highest_concentration(Groningen, amount, radius = 200,
 grid_distance = 25)

# Look for coordinates with even higher concentrations in the
# neighborhood of the coordinates with the highest concentration
hc1_nghb <- neighborhood_gh_search(hc1, max.call = 7000)
print(hc1_nghb)

# Create map with geohashes above the lowerbound
# The highest concentration lies in one of the geohashes
plot(hc1)

# Create map with highest concentration
plot(hc1_nghb)
} # }
```
