# Benchmark hotspot implementations on a simulated 500k building portfolio.
# This script is intentionally not run by CRAN.

library(spatialrisk)

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
}

set.seed(20260503)

n <- 500000L
radius <- 200

# Use the included Dutch insurance locations as an empirical spatial template.
# Resampling and adding small metric jitter gives a building-like point cloud
# over the Netherlands without requiring external data.
base <- insurance[, c("lon", "lat")]
base <- base[complete.cases(base), ]
sampled <- base[sample.int(nrow(base), n, replace = TRUE), ]

metric <- convert_crs_df(sampled, crs_from = 4326, crs_to = 3035,
                         lon_from = "lon", lat_from = "lat",
                         lon_to = "x", lat_to = "y")
metric$x <- metric$x + stats::rnorm(n, mean = 0, sd = 35)
metric$y <- metric$y + stats::rnorm(n, mean = 0, sd = 35)

portfolio <- convert_crs_df(metric[, c("x", "y")],
                            crs_from = 3035, crs_to = 4326,
                            lon_from = "x", lat_from = "y",
                            lon_to = "lon", lat_to = "lat")
portfolio$amount <- round(stats::rlnorm(n, meanlog = 5, sdlog = 1))

run_benchmark <- function(label, expr) {
  gc()
  timing <- system.time(result <- force(expr))
  cat("\n", label, "\n", sep = "")
  print(timing)
  cat("amount_sum:", result$hotspots$amount_sum[1], "\n")
  invisible(list(result = result, timing = timing))
}

cat("Portfolio rows:", nrow(portfolio), "\n")
cat("Radius:", radius, "meters\n")

grid_result <- run_benchmark(
  "grid hotspot search",
  concentration_hotspot(
    portfolio,
    value = "amount",
    radius = radius,
    cell_size = 100,
    grid_precision = 5,
    method = "grid",
    progress = FALSE
  )
)

observed_result <- run_benchmark(
  "observed-points hotspot search",
  concentration_hotspot(
    portfolio,
    value = "amount",
    radius = radius,
    progress = FALSE,
    method = "observed"
  )
)

continuous_result <- run_benchmark(
  "continuous hotspot search",
  concentration_hotspot(
    portfolio,
    value = "amount",
    radius = radius,
    cell_size = 100,
    progress = FALSE
  )
)

summary <- data.frame(
  method = c("grid", "observed", "continuous"),
  elapsed = c(grid_result$timing[["elapsed"]],
              observed_result$timing[["elapsed"]],
              continuous_result$timing[["elapsed"]]),
  amount_sum = c(grid_result$result$hotspots$amount_sum[1],
                 observed_result$result$hotspots$amount_sum[1],
                 continuous_result$result$hotspots$amount_sum[1])
)
summary$speed_vs_grid <- summary$elapsed[summary$method == "grid"] /
  summary$elapsed

cat("\nSummary\n")
print(summary, row.names = FALSE)
