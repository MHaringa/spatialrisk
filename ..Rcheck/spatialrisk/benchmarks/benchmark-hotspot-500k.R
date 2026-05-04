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
  cat("concentration:", result$conc_df$concentration[1], "\n")
  invisible(list(result = result, timing = timing))
}

cat("Portfolio rows:", nrow(portfolio), "\n")
cat("Radius:", radius, "meters\n")

terra_result <- run_benchmark(
  "terra moving-window hotspot",
  concentration_hotspot(
    portfolio,
    value = "amount",
    radius = radius,
    cell_size = 100,
    grid_precision = 5,
    method = "terra",
    progress = FALSE
  )
)

indexed_points_result <- run_benchmark(
  "indexed hotspot, observed points",
  concentration_hotspot(
    portfolio,
    value = "amount",
    radius = radius,
    progress = FALSE,
    method = "indexed"
  )
)

pair_refine_result <- run_benchmark(
  "terra screening with pair-intersection refinement",
  concentration_hotspot(
    portfolio,
    value = "amount",
    radius = radius,
    cell_size = 100,
    refinement_buffer = radius,
    progress = FALSE
  )
)

summary <- data.frame(
  method = c("terra", "indexed_points", "terra_pair_refine"),
  elapsed = c(terra_result$timing[["elapsed"]],
              indexed_points_result$timing[["elapsed"]],
              pair_refine_result$timing[["elapsed"]]),
  concentration = c(terra_result$result$conc_df$concentration[1],
                    indexed_points_result$result$conc_df$concentration[1],
                    pair_refine_result$result$conc_df$concentration[1])
)
summary$speed_vs_terra <- summary$elapsed[summary$method == "terra"] /
  summary$elapsed

cat("\nSummary\n")
print(summary, row.names = FALSE)
