test_that("Dutch spatial datasets keep their expected schema", {
  spatial_data <- list(
    nl_gemeente = c("id", "code", "areaname", "lon", "lat"),
    nl_provincie = c("areaname", "lon", "lat"),
    nl_corop = c("corop_nr", "areaname", "lon", "lat"),
    nl_postcode2 = c("areaname", "lon", "lat"),
    nl_postcode3 = c("areaname", "lon", "lat"),
    nl_postcode4 = c("pc4", "areaname", "city", "lon", "lat")
  )

  for (data_name in names(spatial_data)) {
    x <- get(data_name)
    expect_s3_class(x, "sf")
    expect_true(all(spatial_data[[data_name]] %in% names(x)))
    expect_true(all(is.finite(x$lon)))
    expect_true(all(is.finite(x$lat)))
    expect_equal(sf::st_crs(x)$epsg, 4326)
  }
})

test_that("point datasets keep their expected schema", {
  expect_true(all(c("postcode", "population_pc4", "amount", "lon", "lat") %in%
                    names(insurance)))
  expect_true(is.numeric(insurance$amount))
  expect_true(all(is.finite(insurance$lon)))
  expect_true(all(is.finite(insurance$lat)))

  expect_true(all(c("street", "number", "postal_code", "city", "amount",
                    "lon", "lat") %in% names(Groningen)))
  expect_true(is.numeric(Groningen$amount))
  expect_true(all(is.finite(Groningen$lon)))
  expect_true(all(is.finite(Groningen$lat)))
})

test_that("KNMI station data keeps its expected schema", {
  expect_true(all(c("station", "city", "lon", "lat", "altitude", "X", "Y") %in%
                    names(knmi_stations)))
  expect_true(is.numeric(knmi_stations$station))
  expect_true(all(is.finite(knmi_stations$lon)))
  expect_true(all(is.finite(knmi_stations$lat)))
})

test_that("knmi_historic_data validates input before downloading", {
  expect_error(knmi_historic_data(1950, 1951, progress = FALSE),
               "before 1951")
  expect_error(knmi_historic_data(2020, 2019, progress = FALSE),
               "smaller than or equal")
  expect_error(knmi_historic_data(2019.5, 2020, progress = FALSE),
               "single whole year")
  expect_error(knmi_historic_data(2019, 2020, stations = 999,
                                  progress = FALSE),
               "Unknown KNMI station")
  expect_error(knmi_historic_data(2019, 2020, progress = NA),
               "`progress` must be")
})
