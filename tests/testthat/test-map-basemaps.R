map_basemap_calls <- function(map, method) {
  if (inherits(map, "mapview")) map <- map@map
  Filter(function(x) identical(x$method, method), map$x$calls)
}

map_basemap_providers <- function(map) {
  calls <- map_basemap_calls(map, "addProviderTiles")
  unique(vapply(calls, function(x) x$args[[1]], character(1)))
}

map_basemap_controls <- function(map) {
  calls <- map_basemap_calls(map, "addLayersControl")
  lapply(calls, function(x) as.character(x$args[[1]]))
}

test_that("point maps expose the requested basemaps without changing global options", {
  skip_if_not_installed("mapview")
  providers <- c("Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldImagery")
  before <- mapview::mapviewGetOption("basemaps")
  df <- Groningen[1:5, ]

  default <- map_points(df, value = "amount")
  expect_identical(map_basemap_providers(default), providers)
  expect_identical(map_basemap_controls(default)[[1]], providers)
  for (provider in providers) {
    custom <- map_points(df, basemaps = provider)
    expect_identical(map_basemap_providers(custom), provider)
  }
  expect_identical(mapview::mapviewGetOption("basemaps"), before)
})

test_that("hotspot maps apply basemaps to every overlay and diagnostic plot", {
  skip_if_not_installed("mapview")
  providers <- c("Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldImagery")
  portfolio <- Groningen[1:20, c("lon", "lat", "amount")]
  hotspot <- concentration_hotspot(portfolio, value = "amount", method = "grid",
                                    grid_spacing = 50, progress = FALSE)

  for (type in c("concentration", "focal", "rasterized", "updated_focal")) {
    default <- plot(hotspot, type = type)
    expect_identical(map_basemap_providers(default), providers)
    expect_true(all(vapply(map_basemap_controls(default),
                           function(x) identical(x, providers), logical(1))))
    custom <- plot(hotspot, type = type, basemaps = "Esri.WorldImagery")
    expect_identical(map_basemap_providers(custom), "Esri.WorldImagery")
  }
})

test_that("prepared and selected workflow maps share the same basemap choices", {
  skip_if_not_installed("mapview")
  providers <- c("Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldImagery")
  model <- prepare_spatialrisk(Groningen[1:20, ], value = "amount")
  states <- list(model,
                 select_candidates(model, progress = FALSE),
                 select_candidates(model, method = "observed", progress = FALSE))

  for (state in states) {
    expect_identical(map_basemap_providers(plot(state)), providers)
    expect_identical(map_basemap_providers(plot(state, basemaps = "OpenStreetMap")),
                     "OpenStreetMap")
  }
})

test_that("interactive choropleths expose the same basemaps and allow overrides", {
  skip_if_not_installed("tmap")
  providers <- c("Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldImagery")
  before <- tmap::tmap_mode()
  on.exit(suppressMessages(tmap::tmap_mode(before)), add = TRUE)
  data <- nl_provincie[1:3, ]
  data$output <- 1:3

  map <- tmap::tmap_leaflet(choropleth(data, mode = "view"))
  expect_identical(map_basemap_controls(map)[[1]], providers)
  custom <- tmap::tmap_leaflet(choropleth(data, mode = "view",
                                        basemaps = c("Esri.WorldImagery", "OpenStreetMap")))
  expect_identical(map_basemap_controls(custom)[[1]],
                   c("Esri.WorldImagery", "OpenStreetMap"))
})

test_that("legacy Leaflet plots use the first provider initially", {
  skip_if_not_installed("leaflet")
  providers <- c("Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldImagery")
  for (method in c("plot.conc", "plot.neighborhood")) {
    defaults <- eval(formals(get(method))$providers)
    expect_identical(defaults, providers)
    map <- add_providers_to_map(leaflet::leaflet(), defaults)
    expect_identical(map_basemap_providers(map$map), providers)
    expect_identical(map$used_providers, providers)
    hidden <- map_basemap_calls(map$map, "hideGroup")[[1]]$args[[1]]
    expect_identical(hidden, providers[-1])
  }
  fallback <- add_providers_to_map(leaflet::leaflet(), NULL)
  expect_identical(fallback$used_providers, "OpenStreetMap")
})
