# Fixed-radius concentration analysis

## Motivation

Fixed-radius concentration can be formulated as a weighted
circle-placement problem: given point locations with associated weights
and a disk of fixed radius, find the centre of the disk that maximises
the total weight enclosed. This is a classical spatial optimisation
problem from computational geometry.

In its simplest form, the question is: where can I place a 200 metre
circle so that the total insured value inside that circle is as large as
possible?

In an exposure portfolio, the point weights might represent insured
value, population, asset value, infrastructure value, or another
quantity observed at point locations. Insurance concentration analysis
is one practical application: the same fixed-radius problem can be
interpreted as finding the largest local accumulation of insured value.

The package does not impose a probabilistic model. It computes
deterministic spatial aggregates from observed point locations and
values. The workflows and parameter choices shown in this vignette are
illustrative examples of the computational building blocks, not a
prescribed methodology or process for any particular organisation.

## Example portfolio

The examples below use the included `Groningen` data. The same functions
can be applied to larger portfolios.

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
exposure measure, or another portfolio value. The 200 metre radius used
below is illustrative; the relevant radius depends on the analytical or
reporting context.

## Quick example: find the largest concentration

The main applied workflow is
[`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md).
It takes point-level exposure data, a value column, and a radius, and
returns the circle centre with the largest fixed-radius sum.

``` r

hotspot <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  cell_size = 100,
  progress = FALSE
)

hotspot
#> <hotspot>
#> Number of hotspots: 1 
#> Radius: 200 meters
#> Value: amount 
#> 
#>   id      lon      lat amount_sum
#> 1  1 6.547331 53.23659      64438
```

The reported `amount_sum` is the sum of `amount` within 200 metres of
the selected centre. The default `method = "continuous"` searches for a
centre that may lie anywhere in space, not only on an observed building
location.

``` r

plot(hotspot)
```

## Understanding the result

A hotspot result contains two main components:

- `hotspots`: the selected centre coordinates and the fixed-radius sum;
- `contributing_points`: the observations inside the selected hotspot
  radius.

``` r

hotspot$hotspots
#>   id      lon      lat amount_sum
#> 1  1 6.547331 53.23659      64438

head(hotspot$contributing_points[, c("id", "data_row", "lon", "lat",
                                     "amount", "amount_sum")])
#>   id data_row      lon      lat amount amount_sum
#> 1  1     1492 6.545297 53.23569    148      64438
#> 2  1     4703 6.545482 53.23547    132      64438
#> 3  1    18287 6.545429 53.23546    130      64438
#> 4  1    19958 6.545392 53.23543    138      64438
#> 5  1    22587 6.545493 53.23545    142      64438
#> 6  1       19 6.544724 53.23646    411      64438
```

This separation between the hotspot centre and the contributing
observations is important in applied insurance work. It allows the
result to be inspected, mapped, and reconciled with the underlying
portfolio. The `data_row` column identifies the row in the original
input data.

## Inspecting contributing locations

The observations assigned to the selected hotspot are stored directly in
the hotspot object. This is the safest way to audit the reported
hotspot, because it uses the exact contributing observations returned by
the search.

``` r

head(hotspot$contributing_points)
#>   id data_row      lon      lat amount distance_m amount_sum
#> 1  1     1492 6.545297 53.23569    148   168.9837      64438
#> 2  1     4703 6.545482 53.23547    132   175.7036      64438
#> 3  1    18287 6.545429 53.23546    130   178.8321      64438
#> 4  1    19958 6.545392 53.23543    138   183.0865      64438
#> 5  1    22587 6.545493 53.23545    142   176.7874      64438
#> 6  1       19 6.544724 53.23646    411   174.6183      64438
nrow(hotspot$contributing_points)
#> [1] 208
sum(hotspot$contributing_points$amount)
#> [1] 64438
```

The lower-level function
[`points_within_radius()`](https://mharinga.github.io/spatialrisk/reference/points_within_radius.md)
applies the same local inclusion rule around a specified centre. It is
useful for checking a known or externally specified location.

``` r

known_centre_points <- points_within_radius(
  portfolio,
  lon_center = 6.5549,
  lat_center = 53.1942,
  radius = 200
)

head(known_centre_points)
#> # A tibble: 6 × 4
#>     lon   lat amount distance_m
#>   <dbl> <dbl>  <dbl>      <dbl>
#> 1  6.56  53.2    523       57.5
#> 2  6.56  53.2    513       64.5
#> 3  6.56  53.2    515       70.3
#> 4  6.55  53.2    246       70.6
#> 5  6.56  53.2    768       76.6
#> 6  6.55  53.2    238       79.2
nrow(known_centre_points)
#> [1] 110
sum(known_centre_points$amount)
#> [1] 25668
```

The returned data contains the observations that contribute to this
specified local fixed-radius sum. This makes the aggregate traceable
back to the underlying policies or locations.

## Calculating sums at specified locations

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
locations, at externally specified coordinates, or at candidate centres
created in a custom workflow. In this sense,
[`points_within_radius()`](https://mharinga.github.io/spatialrisk/reference/points_within_radius.md)
and
[`radius_sum()`](https://mharinga.github.io/spatialrisk/reference/radius_sum.md)
are supporting functions: they provide transparency and flexibility
around the main hotspot search.

## Continuous vs observed centres

The maximum fixed-radius circle does not generally need to be centred on
one of the insured locations. This distinction matters because a circle
placed between several buildings may cover a larger total value than any
circle centred exactly on a building.

`spatialrisk` therefore distinguishes two useful search strategies:

- `method = "continuous"`: the centre may lie anywhere in space;
- `method = "observed"`: candidate centres are limited to observed point
  locations.

The observed-points method is fast and deterministic, and is useful as a
benchmark. It can, however, miss a higher concentration when the best
circle centre lies between buildings.

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
#> continuous  1 6.547331 53.23659      64438
#> observed    1 6.547288 53.23664      64172
```

In this example the continuous hotspot has a higher `amount_sum` than
the observed-points hotspot. The example should be read as a
demonstration of the methodological distinction: restricting centres to
observed locations changes the optimisation problem.

``` r

plot(hotspot_continuous)
```

``` r

plot(hotspot_observed)
```

The original grid-refinement workflow remains available with
`method = "grid"`. In that method, `grid_spacing` controls the local
grid refinement. For the default `method = "continuous"`, `grid_spacing`
is only used if the local pair-refinement subset is too large and the
function falls back to grid refinement.

## Multiple hotspots

The argument `n_hotspots` gives the number of hotspots to return. When
`n_hotspots > 1`, hotspots are selected sequentially: after the first
hotspot has been found, its contributing observations are removed before
the next hotspot is searched for. This gives non-overlapping hotspot
assignments.

``` r

hotspot_top2 <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  cell_size = 100,
  progress = FALSE,
  n_hotspots = 2
)

hotspot_top2$hotspots
#>   id      lon      lat amount_sum
#> 1  1 6.547331 53.23659      64438
#> 2  2 6.523411 53.23094      57977
```

The first hotspot addresses the single-circle maximum concentration
problem. The second hotspot is the largest hotspot in the remaining
portfolio after removing the observations assigned to the first hotspot.
This greedy procedure is useful for reporting several distinct local
accumulations, but it should not be interpreted as a globally optimal
joint placement of several circles.

``` r

plot(hotspot_top2)
```

## How the search works

The high-level hotspot workflow can also be run step by step. This is
useful when the intermediate candidate selection needs to be inspected
before the final hotspot is optimised.

The state supplied to
[`optimize_hotspot()`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
determines where geometric candidate centres are generated. A prepared
state has not been screened and therefore represents the full active
portfolio as the candidate-generation universe. A state returned by
[`select_candidates()`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
represents the screened candidate regions. In either case, retained
candidate centres are evaluated against the complete active portfolio:
candidate selection does not create a subportfolio for scoring.

Conceptually, the search has four stages:

- coarse spatial screening identifies promising regions;
- candidate regions are retained using an automatically estimated lower
  bound;
- candidate centres are generated or refined within those regions;
- exact radius sums are evaluated for candidate centres and the best
  result is returned.

``` r

model <- prepare_spatialrisk(portfolio, value = "amount", radius = 200,
                             cell_size = 100)
model <- select_candidates(model, progress = FALSE)
step_hotspot <- optimize_hotspot(model, n_hotspots = 2, progress = FALSE)

step_hotspot$hotspots
#>   id      lon      lat amount_sum
#> 1  1 6.547331 53.23659      64438
#> 2  2 6.523411 53.23094      57977
```

For small validation problems, candidate screening can be omitted
deliberately. The direct route evaluates observed centres and the valid
pairwise circle-intersection centres generated from the complete active
portfolio. The screened route is the normal production workflow used by
[`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md).

``` r

validation_portfolio <- portfolio[1:200, ]
validation_model <- prepare_spatialrisk(
  validation_portfolio,
  value = "amount",
  radius = 200,
  cell_size = 100
)

# Full geometric reference search
full <- optimize_hotspot(validation_model, progress = FALSE)

# Screened production search
screened <- validation_model |>
  select_candidates(progress = FALSE) |>
  optimize_hotspot(progress = FALSE)

full$hotspots
#>   id      lon      lat amount_sum
#> 1  1 6.554816 53.19424       1315
screened$hotspots
#>   id      lon      lat amount_sum
#> 1  1 6.558472 53.19492       1315
```

The full route has pairwise computational cost and is intended for small
portfolios, diagnostics, and methodological validation. It does not
silently switch to grid refinement when the number of points exceeds
`max_refinement_points`; instead, it warns that the requested complete
search may be expensive.

Calling `plot(model)` after
[`prepare_spatialrisk()`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
but before candidate selection shows the rasterised portfolio sum per
cell. After
[`select_candidates()`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md),
`plot(model)` shows only the focal candidate cells above the selected
lower bound.

``` r

prepared <- prepare_spatialrisk(portfolio, value = "amount", radius = 200,
                                cell_size = 100)
plot(prepared)
```

``` r


selected <- select_candidates(prepared, progress = FALSE)
plot(selected)
```

The lower bound can also be supplied explicitly, for example
`select_candidates(model, threshold = 1000)`. When `threshold = NULL`,
the automatic lower bound is deliberately conservative. The function
first takes the highest cells from the focal raster. For those cells it
runs a small local refinement step and uses the best refined
concentration as the lower bound. Candidate cells are then all focal
cells whose moving-window sum is at least this lower bound. The
candidate map is therefore an inspection view of where the next hotspot
may be found, not a fixed list of final hotspots.

When `n_hotspots > 1`, the search is repeated. After the first hotspot
has been found, its contributing observations are removed from the
remaining portfolio and the screening, candidate selection, and
refinement steps are run again for the next hotspot. This is why a
candidate map that currently shows, for example, five focal cells can
still lead to ten hotspots when
`optimize_hotspot(model, n_hotspots = 10)` is used: the five cells
describe the first search iteration only.

The default continuous method uses terra rasterisation and focal sums
for the screening step. It then refines all candidate areas above the
lower bound by evaluating observed local points and the circle centres
implied by local point pairs. The point-to-cell assignment created
during preparation is reused to retrieve points from nearby raster
cells, rather than scanning the complete portfolio separately for every
focal candidate cell. Within each hotspot iteration, exact candidate
evaluations share one spatial lookup over the active portfolio; exact
distances are calculated only for points from potentially intersecting
cells. If the local refinement subset is larger than
`max_refinement_points`, the function falls back to grid refinement for
that iteration.

The focal moving window is deliberately wider than the requested radius
by one raster-cell diagonal. Consequently, with non-negative values, the
focal sum is an upper bound for the exact radius sum of every centre
located in that cell: all source cells that could contribute to such a
centre are included. The automatically estimated threshold is a lower
bound obtained from a feasible preliminary centre. A raster cell whose
focal upper bound is below that threshold cannot contain a better centre
and need not be refined.

For the remaining local point pairs, both radius-circle intersection
centres are constructed geometrically. Each of those two centres is
assigned to a raster cell using terra, and the centres are screened
separately. Only a centre whose own cell passed focal screening receives
the more expensive exact radius evaluation. This is more selective than
retaining both centres merely because one of them lies in a candidate
cell. Every retained centre is then evaluated against the full remaining
active portfolio, not only against the local points used to generate it.
Points outside the candidate-generation subset therefore still
contribute whenever they lie within the radius.

This additional centre-level pruning is used only with non-negative
values and the default automatically estimated lower bound. For a
user-supplied threshold or negative values, the implementation does not
rely on this upper-bound argument and uses the broader refinement route.
The direct validation route `optimize_hotspot(prepare_spatialrisk(...))`
also remains unfiltered and evaluates the complete geometric candidate
set.

## Polygon reporting

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

The radius should be chosen from the application context. In applied
concentration analysis, it may follow from a scenario definition,
reporting objective, risk appetite definition, hazard radius, service
radius, or another domain-specific choice. The package computes the
requested spatial aggregates; the analyst determines which exposure
measure, radius, portfolio scope, and assumptions are appropriate for
the application.

The coordinate columns supplied to the functions are assumed to be
longitude and latitude in EPSG:4326 unless specified otherwise. Distance
calculations for hotspot optimisation are performed in a projected
coordinate reference system with metre units. The default
`crs_metric = 3035` is suitable for Europe-wide applications; for other
regions, use a metric CRS appropriate to the study area.

For the `continuous` and `grid` methods, `cell_size` controls the
initial screening resolution. Smaller cells give a finer screening
surface but increase computation time. The value should be positive and
no larger than the radius. For `radius = 200`, a `cell_size` such as 100
metres is a practical starting point.

For large portfolios, it is useful to keep a reproducible record of:

- the input portfolio and value column;
- the radius;
- the coordinate reference assumptions;
- the search parameters used for hotspot detection;
- the observations contributing to the reported hotspot.

## Mathematical background

The concentration hotspot problem in `spatialrisk` can be interpreted as
a fixed-radius circle-placement problem. Given a set of insured
locations, such as buildings or other point-represented risks, each
location has an associated value, for example insured amount, exposure,
premium, or loss. The objective is to find the location of a circle with
fixed radius that maximizes the total value of the points contained in
that circle.

This problem is closely related to the circle placement problem studied
by Chazelle and Lee (1986). In their formulation, a set of weighted
points in the plane is given and a disk of fixed radius must be placed
such that the total covered weight is maximized. This provides the
theoretical basis for using boundary and pair-intersection geometry.

For the underlying continuous fixed-radius problem, an optimum can be
represented by a centre associated with the relevant point and
circle-boundary intersection geometry under the usual assumptions:
observations are points, weights are non-negative, distances are
Euclidean in a projected coordinate reference system, and the radius is
fixed. The candidate set consisting of observed point locations and the
intersections of radius-`r` circles around pairs of observations is
sufficient for the first single-circle optimum under those assumptions.

The practical `continuous` implementation uses spatial screening and
local refinement to avoid evaluating the full candidate set
indiscriminately. Computational settings such as `cell_size`, the
candidate lower bound, and `max_refinement_points` determine how
extensively the candidate space is explored. The pair-intersection
refinement is exact within the screened local candidate areas; it is not
the same as evaluating every possible pair-intersection candidate
globally in every call.

The lower-level call `optimize_hotspot(prepare_spatialrisk(...))`
provides that complete geometric candidate search for the active
portfolio. For the first hotspot, under the point, non-negative-weight,
fixed-radius, and projected-Euclidean assumptions stated above, this is
the full finite candidate characterisation of the one-disk problem. This
statement does not apply to the screened workflow, grid fallback, or the
joint placement of multiple circles.

For insurance applications this is useful because the method directly
targets accumulation risk: the maximum total value that can be found
within a specified distance of any location. This may be used, for
example, to identify local concentrations of insured building values,
exposed sums insured, or other portfolio-level risk measures.

For multiple hotspots, `spatialrisk` follows a greedy approach: after
the first hotspot is selected, its covered points are removed and the
next hotspot is computed on the remaining portfolio. Each step solves
the corresponding single-hotspot search on the remaining data according
to the selected method and settings, but the sequence is not necessarily
globally optimal as a joint multi-circle optimisation problem.

## Reference

Chazelle, B. M. and Lee, D. T. (1986). On a circle placement problem.
Computing, 36(1–2), 1–16.
[doi:10.1007/BF02238188](https://doi.org/10.1007/BF02238188).
