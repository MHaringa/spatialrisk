library(spatialrisk)
context("haversine")


test_that("return correct output", {
  uit <- haversine(52, 6, 53, 7)
  expect_equal(round(uit, 0), round(130321.2, 0))
})

test_that("return NA for missing coordinates", {
  uit <- expect_warning(
    haversine(c(52, 51), c(6, 6.1), c(53, 51), c(NA, 6)),
    NA
  )
  expect_equal(sum(is.na(uit)), 1)
})

test_that("coordinate vectors must have equal length", {
  expect_error(
    haversine(c(52, 51), 6, c(53, 51), c(7, 8)),
    "same length"
  )
})

test_that("antipodal points return a finite distance", {
  uit <- haversine(0, 0, 0, 180)
  expect_true(is.finite(uit))
  expect_equal(uit, pi * 6378137, tolerance = 1e-7)
})
