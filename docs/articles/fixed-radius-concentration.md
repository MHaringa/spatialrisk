# Fixed-radius concentration analysis

## Motivation

Insurance portfolios often contain many point-level exposures, such as
buildings or policies with an insured amount. For concentration risk
management, the relevant question is not only the total portfolio value,
but also how much value can accumulate locally.

A common applied task is therefore to determine the largest total value
within a circle of fixed radius. This type of calculation is useful when
internal risk limits or regulatory requirements define concentration in
terms of exposure within a specified distance, for example a 200 metre
radius.

The purpose of `spatialrisk` is to make this workflow reproducible in R:

- inspect which observations fall within a radius;
- calculate fixed-radius sums around selected target locations;
- identify concentration hotspots;
- summarise point-level values to polygons for reporting.

The package does not impose a probabilistic model. It computes
deterministic spatial aggregates from observed point locations and
values.

## Example portfolio

The examples below use the included `Groningen` data. For speed in this
vignette, we use a small subset. The same functions can be applied to
larger portfolios.

``` r

library(spatialrisk)

portfolio <- Groningen
portfolio <- portfolio[, c("lon", "lat", "amount")]

head(portfolio)
#> # A tibble: 6 × 3
#>     lon   lat amount
#>   <dbl> <dbl>  <dbl>
#> 1  6.57  53.2     24
#> 2  6.55  53.2     33
#> 3  6.57  53.2     48
#> 4  6.56  53.2      7
#> 5  6.57  53.2     16
#> 6  6.56  53.2     28
```

The `amount` column is the value to be accumulated within each radius.
In an insurance application this could represent an insured amount,
exposure measure, or another portfolio value.

## Inspect observations within a radius

Before searching for a maximum, it is useful to inspect the local
aggregation rule. The following call identifies all points within 200
metres of a chosen centre.

``` r

local_points <- points_within_radius(
  portfolio,
  lon_center = 6.5549,
  lat_center = 53.1942,
  radius = 200
)

head(local_points)
#> # A tibble: 6 × 4
#>     lon   lat amount distance_m
#>   <dbl> <dbl>  <dbl>      <dbl>
#> 1  6.56  53.2    523       57.5
#> 2  6.56  53.2    513       64.5
#> 3  6.56  53.2    515       70.3
#> 4  6.55  53.2    246       70.6
#> 5  6.56  53.2    768       76.6
#> 6  6.55  53.2    238       79.2
nrow(local_points)
#> [1] 110
sum(local_points$amount)
#> [1] 25668
```

The returned data contains the observations that contribute to this
local fixed-radius sum. This is helpful for auditability: the aggregate
can be traced back to the underlying policies or locations.

## Calculate fixed-radius sums around target locations

The same operation can be repeated for several target locations with
[`radius_sum()`](https://mharinga.github.io/spatialrisk/reference/radius_sum.md).
Here, the target locations are the first five observations in the
portfolio.

``` r


targets <- portfolio[1:5, c("lon", "lat")]

target_sums <- radius_sum(
  targets = targets,
  reference = portfolio,
  value = "amount",
  radius = 200,
  progress = FALSE,
  result_col = "amount_200m"
)

target_sums
#> # A tibble: 5 × 3
#>     lon   lat amount_200m
#>   <dbl> <dbl>       <dbl>
#> 1  6.57  53.2        8612
#> 2  6.55  53.2       16704
#> 3  6.57  53.2        9120
#> 4  6.56  53.2        7970
#> 5  6.57  53.2        8633
```

The `targets` and `reference` arguments are separated deliberately. This
makes it possible to evaluate concentration at existing policy
locations, at grid points, or at any other candidate centres.

## Identify a concentration hotspot

The main applied task is to find the location where the fixed-radius sum
is largest.
[`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md)
searches for such a centre and returns both the hotspot and the
contributing observations. The default `method = "continuous"` searches
for a centre that may lie between buildings. Internally, it uses a
coarse spatial screening step followed by local pair-intersection
refinement. If the local refinement subset is larger than
`max_refinement_points`, the function falls back to grid refinement.

``` r


hotspot <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  cell_size = 100,
  progress = FALSE, 
  top_n = 2
)

plot(hotspot)
```

The reported `amount_sum` is the sum of `amount` within the 200 metre
circle around the selected centre. The argument `top_n` gives the number
of hotspots to return. When `top_n > 1`, the points contributing to the
first hotspot are removed before the next hotspot is searched for. This
gives non-overlapping hotspot assignments. The contributing observations
are available as follows:

``` r

head(hotspot$contributing_points[, c("id", "data_row", "lon", "lat", "amount", "amount_sum")])
#>   id data_row      lon      lat amount amount_sum
#> 1  1     1492 6.545297 53.23569    148      64308
#> 2  1     4703 6.545482 53.23547    132      64308
#> 3  1    18287 6.545429 53.23546    130      64308
#> 4  1    19958 6.545392 53.23543    138      64308
#> 5  1    22587 6.545493 53.23545    142      64308
#> 6  1       19 6.544724 53.23646    411      64308
```

This separation between the hotspot centre and the contributing
observations is important in applied insurance work. It allows the
result to be inspected, mapped, and reconciled with the underlying
portfolio.

The same workflow can also be run step by step. This is useful when the
intermediate candidate selection needs to be inspected before the final
hotspot is optimised.

``` r


model <- prepare_spatialrisk(portfolio, value = "amount", radius = 200,
                             cell_size = 100)
model <- select_candidates(model, progress = FALSE)
step_hotspot <- optimize_hotspot(model, top_n = 2, progress = FALSE)
```

Calling `plot(model)` before candidate selection shows the rasterised
portfolio sum per cell. After
[`select_candidates()`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md),
`plot(model)` shows only the focal candidate cells above the
automatically estimated lower bound. The lower bound can also be
supplied explicitly, for example
`select_candidates(model, threshold = 1000)`.

The automatic lower bound is deliberately conservative. The function
first takes the highest cells from the focal raster. For those cells it
runs a small local refinement step and uses the best refined
concentration as the lower bound. Candidate cells are then all focal
cells whose moving-window sum is at least this lower bound. The
candidate map is therefore an inspection view of where the next hotspot
may be found, not a fixed list of final hotspots.

When `top_n > 1`, the search is repeated. After the first hotspot has
been found, its contributing observations are removed from the remaining
portfolio and the screening, candidate selection, and refinement steps
are run again for the next hotspot. This is why a candidate map that
currently shows, for example, five focal cells can still lead to ten
hotspots when `optimize_hotspot(model, top_n = 10)` is used: the five
cells describe the first search iteration only.

The default continuous method may place the hotspot centre between
buildings. This is important: the circle with the largest total value
often does not have its centre exactly on one observed building, but
somewhere between several buildings.

The package also includes an observed-points method, available by
setting `method = "observed"` in
[`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md).
This method searches only observed point locations as possible circle
centres. It is useful as a fast and deterministic benchmark, but it can
miss a higher concentration when the best circle centre lies between
buildings.

``` r

hotspot_continuous <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  cell_size = 100,
  progress = FALSE
)

hotspot_observed <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  method = "observed",
  progress = FALSE
)

rbind(
  continuous = hotspot_continuous$hotspots,
  observed = hotspot_observed$hotspots
)
#>            id      lon      lat amount_sum
#> continuous  1 6.547323 53.23663      64308
#> observed    1 6.547288 53.23664      64172
```

In this example the continuous hotspot has a higher `amount_sum` than
the observed-points hotspot, because the observed method only evaluates
existing building locations as candidate centres.

``` r


plot(hotspot_continuous)
```

``` r

plot(hotspot_observed)
```

The original grid-refinement workflow remains available with
`method = "grid"`. In that method, `grid_precision` controls the local
grid refinement. For the default `method = "continuous"`,
`grid_precision` is only used if the local pair-refinement subset is too
large and the function falls back to grid refinement.

## Reporting by polygon

Fixed-radius concentration is a point-level calculation. For
communication and reporting, it is often useful to summarise values by
administrative or portfolio regions. The function
[`summarise_points_by_polygon()`](https://mharinga.github.io/spatialrisk/reference/summarise_points_by_polygon.md)
joins point data to polygons and applies a summary function.

``` r

province_summary <- summarise_points_by_polygon(
  polygons = nl_provincie,
  points = insurance,
  value = "amount",
  fun = sum,
  outside = "ignore"
)

sf::st_drop_geometry(province_summary)[, c("areaname", "amount_sum")]
#>         areaname amount_sum
#> 1        Drenthe   56766689
#> 2      Flevoland   55795037
#> 3      Friesland   78581984
#> 4     Gelderland  269468412
#> 5      Groningen  106580080
#> 6        Limburg  140680821
#> 7  Noord-Brabant  377776132
#> 8  Noord-Holland  593255924
#> 9     Overijssel  148939513
#> 10       Utrecht  226377123
#> 11       Zeeland   82251913
#> 12  Zuid-Holland  697040028
```

This polygon summary answers a different question from the hotspot
search. The hotspot search is based on circles with fixed radius; the
polygon summary is based on predefined administrative boundaries. Both
can be useful, but they should not be interpreted as the same measure.

## Practical considerations

The radius should be chosen from the application context. In insurance
concentration analysis, it may follow from regulation, internal risk
appetite, or a scenario definition.

The coordinate columns supplied to the functions are assumed to be
longitude and latitude in EPSG:4326 unless specified otherwise. Distance
calculations for point-level radius operations are performed in metres.

For large portfolios, it is useful to keep a reproducible record of:

- the input portfolio and value column;
- the radius;
- the coordinate reference assumptions;
- the search parameters used for hotspot detection;
- the observations contributing to the reported hotspot.

## Relation to circle placement problems

The concentration hotspot problem in `spatialrisk` can be interpreted as
a fixed-radius circle placement problem. Given a set of insured
locations, such as buildings or other point-represented risks, each
location has an associated value, for example insured amount, exposure,
premium, or loss. The objective is to find the location of a circle with
fixed radius that maximizes the total value of the points contained in
that circle.

This problem is closely related to the circle placement problem studied
by Chazelle and Lee (1986). In their formulation, a set of weighted
points in the plane is given and a disk of fixed radius must be placed
such that the total covered weight is maximized. This provides the
theoretical basis for the pairwise-intersection method implemented here.

The pairwise-intersection method avoids evaluating all possible grid
locations. Instead, it generates candidate centers from observed point
locations and from the intersections of radius-r circles around pairs of
observations. Under the assumptions that observations are points,
weights are non-negative, distances are Euclidean in a projected
coordinate reference system, and the radius is fixed, this candidate set
is sufficient to find the exact optimum for the first hotspot.

For insurance applications this is useful because the method directly
targets accumulation risk: the maximum total value that can be found
within a specified distance of any location. This may be used, for
example, to identify local concentrations of insured building values,
exposed sums insured, or other portfolio-level risk measures.

For multiple hotspots, `spatialrisk` follows a greedy approach: after
the first hotspot is selected, its covered points are removed and the
next hotspot is computed on the remaining portfolio. Each step is exact
under the assumptions above, but the sequence is not necessarily
globally optimal as a joint multi-circle optimization problem.
