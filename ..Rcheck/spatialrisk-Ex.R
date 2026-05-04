pkgname <- "spatialrisk"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('spatialrisk')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("choropleth")
### * choropleth

flush(stderr()); flush(stdout())

### Name: choropleth
### Title: Create choropleth map
### Aliases: choropleth

### ** Examples

test <- summarise_points_by_polygon(nl_provincie, insurance, "amount")
choropleth(test, value = "amount_sum")
choropleth(test, value = "amount_sum", id = "areaname", mode = "view")




cleanEx()
nameEx("haversine")
### * haversine

flush(stderr()); flush(stdout())

### Name: haversine
### Title: Haversine great-circle distance
### Aliases: haversine

### ** Examples

haversine(53.24007, 6.520386, 53.24054, 6.520386)

lat_from <- c(53.24, 52.37)
lon_from <- c(6.52, 4.90)
lat_to   <- c(48.85, 51.92)
lon_to   <- c(2.35, 4.48)
haversine(lat_from, lon_from, lat_to, lon_to)




cleanEx()
nameEx("highest_concentration")
### * highest_concentration

flush(stderr()); flush(stdout())

### Name: highest_concentration
### Title: Highest concentration risk
### Aliases: highest_concentration

### ** Examples

 ## Not run: 
##D # Find highest concentration with a precision of a grid of 25 meters
##D hc1 <- highest_concentration(Groningen, amount, radius = 200,
##D  grid_distance = 25)
##D 
##D # Look for coordinates with even higher concentrations in the
##D # neighborhood of the coordinates with the highest concentration
##D hc1_nghb <- neighborhood_gh_search(hc1, max.call = 7000)
##D print(hc1_nghb)
##D 
##D # Create map with geohashes above the lowerbound
##D # The highest concentration lies in one of the geohashes
##D plot(hc1)
##D 
##D # Create map with highest concentration
##D plot(hc1_nghb)
## End(Not run)




cleanEx()
nameEx("knmi_historic_data")
### * knmi_historic_data

flush(stderr()); flush(stdout())

### Name: knmi_historic_data
### Title: Retrieve historic weather data for the Netherlands
### Aliases: knmi_historic_data

### ** Examples

## Not run: 
##D knmi_historic_data(2015, 2019, stations = c(260, 280))
## End(Not run)




cleanEx()
nameEx("map_points")
### * map_points

flush(stderr()); flush(stdout())

### Name: plot_points
### Title: Create interactive point map
### Aliases: plot_points map_points

### ** Examples

## Not run: 
##D map_points(Groningen, value = "amount")
## End(Not run)




cleanEx()
nameEx("neighborhood_gh_search")
### * neighborhood_gh_search

flush(stderr()); flush(stdout())

### Name: neighborhood_gh_search
### Title: Search for coordinates with higher concentrations within geohash
### Aliases: neighborhood_gh_search

### ** Examples

## Not run: 
##D # Find highest concentration with a precision of a grid of 25 meters
##D hc1 <- highest_concentration(Groningen, amount, radius = 200,
##D  grid_distance = 25)
##D 
##D # Increase the number of calls for more extensive search
##D hc1_nghb <- neighborhood_gh_search(hc1, max.call = 7000, highest_geohash = 1)
##D hc2_nghb <- neighborhood_gh_search(hc1, max.call = 7000, highest_geohash = 2)
##D plot(hc1_nghb)
##D plot(hc2_nghb)
## End(Not run)



cleanEx()
nameEx("radius_sum")
### * radius_sum

flush(stderr()); flush(stdout())

### Name: concentration
### Title: Sum values within a radius around target coordinates
### Aliases: concentration radius_sum

### ** Examples

targets <- data.frame(location = c("p1", "p2"),
                      lon = c(6.561561, 6.561398),
                      lat = c(53.21369, 53.21326))

reference <- data.frame(lon = c(6.5614, 6.5620, 6.5630),
                        lat = c(53.2132, 53.2140, 53.2150),
                        amount = c(10, 20, 15))

radius_sum(targets, reference, value = "amount", radius = 100)




cleanEx()
nameEx("summarise_points_by_polygon")
### * summarise_points_by_polygon

flush(stderr()); flush(stdout())

### Name: points_to_polygon
### Title: Summarise point values by polygon
### Aliases: points_to_polygon summarise_points_by_polygon

### ** Examples

summarise_points_by_polygon(
  polygons = nl_postcode2,
  points = insurance,
  value = "amount",
  fun = sum
)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
