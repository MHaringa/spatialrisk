# Benchmark the terra and indexed hotspot implementations.
# This script is intentionally not run by CRAN.

library(spatialrisk)

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
}

portfolio <- Groningen

if (nrow(portfolio) > 5000) {
  set.seed(1)
  portfolio <- portfolio[sample.int(nrow(portfolio), 5000), ]
}

portfolio <- portfolio[, c("lon", "lat", "amount")]

run_benchmark <- function(label, expr) {
  gc()
  timing <- system.time(result <- force(expr))
  cat("\n", label, "\n", sep = "")
  print(timing)
  cat("concentration:", result$conc_df$concentration[1], "\n")
  invisible(list(result = result, timing = timing))
}

terra_result <- run_benchmark(
  "terra moving-window hotspot",
  concentration_hotspot(
    portfolio,
    value = "amount",
    radius = 200,
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
    radius = 200,
    progress = FALSE,
    method = "indexed"
  )
)

pair_refine_result <- run_benchmark(
  "terra screening with pair-intersection refinement",
  concentration_hotspot(
    portfolio,
    value = "amount",
    radius = 200,
    cell_size = 100,
    refinement_buffer = 200,
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
