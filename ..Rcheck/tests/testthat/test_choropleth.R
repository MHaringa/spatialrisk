library(spatialrisk)
context("choropleth")

make_test_sf <- function() {
  geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
    sf::st_polygon(list(rbind(c(0, 1), c(1, 1), c(1, 2), c(0, 2), c(0, 1)))),
    crs = 4326
  )
  sf::st_sf(area = c("a", "b", "c"),
            output = c(1, 2, 3),
            geometry = geometry)
}

test_that("choropleth returns a tmap object", {
  testthat::skip_if_not_installed("tmap")

  out <- choropleth(make_test_sf(), value = "output", id = "area",
                    mode = "plot")

  expect_s3_class(out, "tmap")
})

test_that("choropleth validates inputs", {
  testthat::skip_if_not_installed("tmap")

  sf_object <- make_test_sf()

  expect_error(choropleth(data.frame(output = 1)), "sf object")
  expect_error(choropleth(sf_object, value = "missing"), "not found")
  expect_error(choropleth(sf_object, id = "missing"), "not found")
  expect_error(choropleth(sf_object, mode = "other"), "should be one of")
  expect_error(choropleth(sf_object, n = 0), "positive integer")
})

test_that("id_name remains a deprecated alias", {
  testthat::skip_if_not_installed("tmap")

  out <- expect_warning(
    choropleth(make_test_sf(), value = "output", id_name = "area"),
    "deprecated"
  )

  expect_s3_class(out, "tmap")
})

test_that("choropleth_ggplot2 is deprecated but still works", {
  testthat::skip_if_not_installed("ggplot2")

  out <- expect_warning(
    choropleth_ggplot2(make_test_sf(), value = output),
    "deprecated"
  )

  expect_s3_class(out, "ggplot")
})
