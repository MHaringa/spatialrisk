# Run from the package root. Does not overwrite manuscript benchmark artifacts.
pkgload::load_all(".", quiet = TRUE)
output <- file.path("dev", "point-cell-screening-results")
dir.create(output, showWarnings = FALSE)

# Import the canonical generators without executing their manuscript workflows.
for (path in c("../paper/benchmark-berlin.R", "../paper/validate-hotspot-search.R")) {
  for (expr in parse(path)) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))) {
      eval(expr)
    }
  }
}

measure <- function(data, label, run, tighten) {
  if (!tighten) {
    testthat::local_mocked_bindings(
      tighten_hotspot_candidate_cells = function(cells, state, metric, value,
                                                  radius, threshold) {
        list(cells = cells, threshold = threshold)
      }, .package = "spatialrisk"
    )
  }
  options(spatialrisk.profile = TRUE)
  gc()
  timing <- system.time({
    model <- prepare_spatialrisk(data, "amount", radius = 200, cell_size = 100)
    model <- select_candidates(model, max_refinement_points = 2500, progress = FALSE)
    result <- optimize_hotspot(model, progress = FALSE)
  })
  profile <- attr(result, "profile")[[1]]
  data.frame(
    portfolio = label, run = run, tightened = tighten, n = nrow(data),
    elapsed_seconds = unname(timing[["elapsed"]]),
    lower_bound = model$candidates$threshold,
    candidate_cells = nrow(model$candidates$cells),
    candidate_points = profile$unique_observed_centres,
    point_pairs = profile$unique_point_pairs,
    hotspot_value = result$hotspots$amount_sum[1],
    contributing_records = nrow(result$contributing_points),
    refinement = attr(result, "refinement_methods")[1]
  )
}

rows <- list()
portfolio <- Groningen[, c("lon", "lat", "amount")]
invisible(measure(portfolio, "warmup", 0L, TRUE))
invisible(measure(portfolio, "warmup", 0L, FALSE))
for (run in 1:5) for (tighten in c(FALSE, TRUE)) {
  rows[[length(rows) + 1L]] <- measure(portfolio, "Groningen", run, tighten)
}
for (n in c(50000L, 500000L)) for (seed in c(1L, 5L)) {
  portfolio <- simulate_berlin_portfolio(n, seed)
  for (tighten in c(FALSE, TRUE)) {
    row <- measure(portfolio, paste0("Berlin-", n, "-seed-", seed), 1L, tighten)
    rows[[length(rows) + 1L]] <- row
    print(row)
  }
}
results <- do.call(rbind, rows)
write.csv(results, file.path(output, "timings.csv"), row.names = FALSE)
paired <- merge(results[results$tightened, ], results[!results$tightened, ],
                by = c("portfolio", "run"))
stopifnot(all(paired$hotspot_value.x == paired$hotspot_value.y),
          all(paired$contributing_records.x == paired$contributing_records.y))
print(aggregate(elapsed_seconds ~ portfolio + tightened, results, median))

options(spatialrisk.profile = FALSE)
validation <- do.call(rbind, lapply(c(50L, 100L, 250L, 500L, 1000L), function(n) {
  do.call(rbind, lapply(1101:1120, function(seed) validate_one_portfolio(n, seed)))
}))
write.csv(validation, file.path(output, "validation.csv"), row.names = FALSE)
stopifnot(all(validation$recovered),
          all(validation$screened_refinement == "pair_intersections"))
print(names(validation))
print(summary(validation))
capture.output(sessionInfo(), file = file.path(output, "session.txt"))
