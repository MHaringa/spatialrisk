# Benchmark the hotspot search methods.
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
  cat("amount_sum:", result$hotspots$amount_sum[1], "\n")
  invisible(list(result = result, timing = timing))
}

grid_result <- run_benchmark(
  "grid hotspot search",
  concentration_hotspot(
    portfolio,
    value = "amount",
    radius = 200,
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
    radius = 200,
    progress = FALSE,
    method = "observed"
  )
)

continuous_result <- run_benchmark(
  "continuous hotspot search",
  concentration_hotspot(
    portfolio,
    value = "amount",
    radius = 200,
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
