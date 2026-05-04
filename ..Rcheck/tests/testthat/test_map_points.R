library(spatialrisk)
context("map_points")

test_that("map_points maps points with and without value", {
  testthat::skip_if_not_installed("mapview")

  df <- Groningen[1:5, ]

  with_value <- map_points(df, value = "amount")
  without_value <- map_points(df)

  expect_s4_class(with_value, "mapview")
  expect_s4_class(without_value, "mapview")
})

test_that("map_points accepts custom coordinate columns and extra arguments", {
  testthat::skip_if_not_installed("mapview")

  df <- data.frame(x = Groningen$lon[1:5],
                   y = Groningen$lat[1:5],
                   amount = Groningen$amount[1:5])

  out <- map_points(df, value = "amount", lon = "x", lat = "y",
                    layer_name = "insured amount", legend = FALSE)

  expect_s4_class(out, "mapview")
})

test_that("map_points validates inputs", {
  df <- Groningen[1:5, ]

  expect_error(map_points(df, value = "missing"), "not found")
  expect_error(map_points(df, lon = "missing"), "not found")
  expect_error(map_points(transform(df, lon = as.character(lon))), "numeric")
  expect_error(map_points(df, crs = NA_real_), "EPSG")
  expect_error(map_points(df, at = "high"), "numeric")
  expect_error(map_points(df, value = c("amount", "amount")), "single")
})

test_that("plot_points remains a deprecated wrapper", {
  testthat::skip_if_not_installed("mapview")

  out <- expect_warning(
    plot_points(Groningen[1:5, ], value = "amount"),
    "deprecated"
  )

  expect_s4_class(out, "mapview")
})
