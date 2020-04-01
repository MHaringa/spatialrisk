library(spatialrisk)
context("haversine")


test_that("return correct output", {
  uit <- haversine(52, 6, 53, 7)
  expect_equal(round(uit, 0), round(130321.2, 0))
  })

test_that("return NA for missing coordinates", {
  uit <- haversine(c(52, 51), c(6, 6.1), c(53,51), c(NA, 6))
  expect_equal(sum(is.na(uit)), 1)
})


