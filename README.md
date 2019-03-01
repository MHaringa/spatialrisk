
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spatialrisk

spatialrisk is an R-package for spatial risk calculations. In
particular, it can be used to determine concentration risk in the
context of Solvency II.

The package offers an effective approach to calculate the *standard
formula* under Solvency II. The *standard formula* under Solvency II
asks companies to report their largest fire concentration in respect of
the fire peril within a radius of 200m. This is the maximum gross sum
insured of the set of buildings fully or partly located within this
radius.

## Installation

You can install spatialrisk from github with:

``` r
# install.packages("devtools")
devtools::install_github("MHaringa/spatialrisk")
```

## Example 1

Find all observations in dataframe `Groningen` within radius of 50m from
center point *(lon,lat) = (6.561561,53.21326)*:

``` r
library(spatialrisk)
points_in_circle(Groningen, lon_center = 6.571561, lat_center = 53.21326, radius = 50)
#>          street number letter suffix postal_code      city      lon
#> 1348 Heresingel      5   <NA>   <NA>      9711EP Groningen 6.571338
#> 1346 Heresingel      3      a   <NA>      9711EP Groningen 6.571005
#> 1349 Heresingel      7   <NA>   <NA>      9711EP Groningen 6.571649
#> 1347 Heresingel      3   <NA>   <NA>      9711EP Groningen 6.570963
#> 1350 Heresingel      9   <NA>   <NA>      9711ER Groningen 6.571712
#> 1345 Heresingel      1      a   <NA>      9711EP Groningen 6.570886
#> 1351 Heresingel     11   <NA>   <NA>      9711ER Groningen 6.571767
#>           lat amount distance_m
#> 1348 53.21351      5   31.36533
#> 1346 53.21334     36   38.07244
#> 1349 53.21361      7   39.45827
#> 1347 53.21339     36   42.45819
#> 1350 53.21365      9   44.28135
#> 1345 53.21334     12   45.92392
#> 1351 53.21367     11   47.79962
```

## Example 2

Find for each row in dataframe `df` the observations in dataframe
`Groningen` within radius of 100m from lon/lat pair. Then the function
takes for the obtained observations the sum of the column `amount`:

``` r
df <- data.frame(location = c("p1", "p2"), 
               lon = c(6.561561, 6.561398), 
               lat = c(53.21369, 53.21326))

concentration(df, Groningen, value = amount, radius = 100)
#>   location      lon      lat concentration
#> 1       p1 6.561561 53.21369          3819
#> 2       p2 6.561398 53.21326          4771
```
