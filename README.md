---
output: github_document
always_allow_html: yes
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# spatialrisk <img src="logo.png" align="right" width="120"/>

<!-- badges: start -->

[![CRAN Status](https://www.r-pkg.org/badges/version/spatialrisk)](https://cran.r-project.org/package=spatialrisk) [![Downloads](https://cranlogs.r-pkg.org/badges/spatialrisk?color=blue)](https://cran.r-project.org/package=spatialrisk)

<!-- badges: end -->

`spatialrisk` provides tools for fixed-radius spatial concentration analysis in R.
The package is aimed at applied workflows in which point-level values must be aggregated locally, for example to identify insurance concentration hotspots under a prescribed radius.

The central question is practical: given a portfolio of point locations with associated values, which locations have the largest total value within a circle of fixed radius?
This type of problem occurs in insurance risk management, but the same operations are useful in other spatial point pattern applications.

## Installation


``` r
install.packages("spatialrisk")

# Development version
remotes::install_github("MHaringa/spatialrisk")
```

## Main operations

The package is intentionally focused on a small set of operations.

1. identify points within a fixed radius;
2. compute fixed-radius sums around target locations;
3. search for locations with maximum local concentration;
4. aggregate point values to polygons for reporting or visualisation.

These operations are building blocks for applied concentration analyses rather than a general spatial modelling framework.

## Applied example

The package includes example address-level data for Groningen.
The column `amount` represents an example value attached to each location.


``` r
library(spatialrisk)

portfolio <- Groningen
head(portfolio[, c("lon", "lat", "amount")])
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

### Points contributing to a local concentration

`points_within_radius()` returns the observations within a given distance from a specified centre.
This is useful for inspecting which policies, buildings, or other exposure points contribute to a local aggregate.


``` r
local_points <- points_within_radius(
  portfolio,
  lon_center = 6.5549,
  lat_center = 53.1942,
  radius = 200
)

nrow(local_points)
#> [1] 110
sum(local_points$amount)
#> [1] 25668
```

### Fixed-radius sums

`radius_sum()` evaluates the same idea for one or more target locations.
The output is the target data with an additional column containing the total value from the reference data within the specified radius.


``` r
targets <- portfolio[1:5, c("lon", "lat")]

radius_sum(
  targets = targets,
  reference = portfolio,
  value = "amount",
  radius = 200,
  progress = FALSE,
  result_col = "amount_200m"
)
#> # A tibble: 5 × 3
#>     lon   lat amount_200m
#>   <dbl> <dbl>       <dbl>
#> 1  6.57  53.2        8612
#> 2  6.55  53.2       16704
#> 3  6.57  53.2        9120
#> 4  6.56  53.2        7970
#> 5  6.57  53.2        8633
```

### Concentration hotspot

`concentration_hotspot()` searches for the centre of a fixed-radius circle with the largest aggregated value.
In an insurance setting this can be used to identify the local portfolio concentration that is relevant for a regulatory or internal risk limit.
By default, `method = "continuous"` searches for a centre that may lie between
buildings. Internally, it uses a coarse spatial screening step followed by
local pair-intersection refinement.


``` r
hotspot <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  cell_size = 100,
  progress = FALSE, 
  top_n = 2
)

hotspot
#> <hotspot>
#> Number of hotspots: 2 
#> Radius: 200 meters
#> Value: amount 
#> 
#>   id      lon      lat amount_sum
#> 1  1 6.547323 53.23663      64308
#> 2  2 6.528279 53.22564      42499
```

The result contains the selected centre coordinates and the corresponding summed value, named from `value`; for example `amount_sum`.
The contributing observations are stored in `hotspot$contributing_points`.

The wrapper can also be decomposed when the candidate-selection step needs to
be inspected.


``` r
model <- prepare_spatialrisk(portfolio, value = "amount", radius = 200,
                             cell_size = 100)
model <- select_candidates(model, progress = FALSE)
step_hotspot <- optimize_hotspot(model, top_n = 2, progress = FALSE)

step_hotspot$hotspots
#>   id      lon      lat amount_sum
#> 1  1 6.547323 53.23663      64308
#> 2  2 6.528279 53.22564      42499
```

Calling `plot(model)` before candidate selection shows the rasterised portfolio
sum per cell. After `select_candidates()`, `plot(model)` shows only the focal
candidate cells above the automatically estimated lower bound. The lower bound
can also be supplied explicitly, for example `select_candidates(model,
threshold = 1000)`.

The default continuous method can place the circle centre between buildings.
For comparison, `method = "observed"` searches only observed point locations as
possible centres and is therefore useful as a fast benchmark.


``` r
observed_hotspot <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  method = "observed",
  progress = FALSE
)

rbind(
  continuous = hotspot$hotspots[1, ],
  observed = observed_hotspot$hotspots
)
#>            id      lon      lat amount_sum
#> continuous  1 6.547323 53.23663      64308
#> observed    1 6.547288 53.23664      64172
```

The original grid-refinement workflow remains available with `method = "grid"`.

## Polygon summaries

Point-level concentration analysis is often followed by reporting at an administrative or portfolio-management level.
For that purpose, `summarise_points_by_polygon()` joins points to polygons and summarises a numeric value.


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

## Scope

`spatialrisk` does not estimate a statistical model and does not assign a probability distribution to the observed values.
It provides deterministic spatial aggregation tools for fixed-radius concentration problems.
Interpretation of the resulting concentration measures remains application-specific.

Core computations are implemented in C++ via Rcpp for efficient evaluation on larger point datasets.

## Vignettes

The main vignette introduces fixed-radius concentration analysis in an applied insurance setting.
A second vignette shows how aggregated values can be displayed with choropleth maps.

## Reference

The fixed-radius hotspot problem is related to the maximum covering location problem described by Church (1974) <doi:10.1007/BF01942293>.

If you use this package in academic work, it can be cited as:

> Haringa, M. (2026). spatialrisk: Spatial concentration and radius-based risk calculations in R.
