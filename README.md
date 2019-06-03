
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
#> Registered S3 methods overwritten by 'ggplot2':
#>   method         from 
#>   [.quosures     rlang
#>   c.quosures     rlang
#>   print.quosures rlang
points_in_circle(Groningen, lon_center = 6.571561, lat_center = 53.21326, radius = 50)
#> # A tibble: 3 x 10
#>   street number letter suffix postal_code city    lon   lat amount
#>   <chr>   <int> <chr>  <chr>  <chr>       <chr> <dbl> <dbl>  <dbl>
#> 1 Heres…      5 <NA>   <NA>   9711EP      Gron…  6.57  53.2      5
#> 2 Heres…      3 a      <NA>   9711EP      Gron…  6.57  53.2     36
#> 3 Heres…     11 <NA>   <NA>   9711ER      Gron…  6.57  53.2     11
#> # … with 1 more variable: distance_m <dbl>
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
#> 1       p1 6.561561 53.21369          2055
#> 2       p2 6.561398 53.21326          2892
```

## Example 3

The package also contains functionality to create choropleths. Typically
in R it is difficult to create choropleths. The functions presented here
attempt to elegantly solve this problem.

The common approach is to first aggregate the data on the level of the
regions in the shapefile and then merging the aggregated data with the
shapefile. This is done by joining on the name. This approach is often
problematic. For example, in case of municipality names in the
Netherlands it is not easy to merge municipality names in the shapefile
with the names in the data set. This is hard because of municipal
reorganizations or differences in punctuation marks in municipality
names. To solve this problem the data is not aggregated on the level of
the regions in the shapefile. Contrary, the functions in this package
detect directly the region containing the coordinates of the underlying
object. This flexible approach makes it easy to create choropleth maps
on different region levels.

The package has the following build-in choropleth maps:

  - nl\_provincie
  - nl\_corop
  - nl\_gemeente
  - nl\_postcode1
  - nl\_postcode2
  - nl\_postcode3
  - nl\_postcode4
  - world\_countries
  - europe\_countries

The insurance dataset contains 30,000 postal codes with their sum
insured, population and the corresponding longitude and latitude. The
following code shows how to create a simple feature object on the
2-digit postcode level. The regions are shaded by the total sum insured
per region.

``` r

gemeente_sf <- choropleth_sf(nl_gemeente, insurance, sum(amount, na.rm = TRUE))
#> 33 points fall not within a polygon.
```

The following code shows how to create a choropleth map based on the
simple feature object obtained in the previous step. There are three
options to create a choropleth map. The first one is using the ggplot2
package:

``` r
spatialrisk::choropleth_ggplot2(gemeente_sf)
```

![](README-example3a-1.png)<!-- -->

The second approach is creating a choropleth map using the tmap package:

``` r
choropleth_tmap(gemeente_sf, mode = "plot")
#> Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3
```

![](README-example3b-1.png)<!-- -->

The third option is to create an interactive map. This can also be
achieved using the tmap package.

``` r
choropleth_tmap(gemeente_sf, mode = "view")
```

![](README-example3d-1.png)<!-- -->
