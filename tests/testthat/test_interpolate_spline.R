library(spatialrisk)
context("interpolate_spline")

test_that("interpolate_spline is deprecated but still returns predictions", {
  testthat::skip_if_not_installed("mgcv")

  set.seed(1)
  observations <- data.frame(
    lon = runif(40, -1, 1),
    lat = runif(40, -1, 1)
  )
  observations$amount <- observations$lon + observations$lat
  targets <- data.frame(lon = c(0.25, 0.75),
                        lat = c(0.25, 0.75))

  out <- expect_warning(
    interpolate_spline(observations, targets, amount, k = 10),
    "deprecated"
  )

  expect_true("amount_pred" %in% names(out))
  expect_equal(nrow(out), nrow(targets))
})
