## Submission

This is version 0.8.2 of spatialrisk, a minor release following 0.8.1.

The main changes are:

* Accelerated single-hotspot continuous refinement with a streaming Rcpp
  angular sweep while retaining exact scoring against the complete active
  portfolio.
* Corrected and documented the conservative raster screening bounds, including
  non-square cells and candidate centres near the portfolio extent.
* Added a complete geometric reference search for small validation problems and
  clarified the distinction between candidate generation and portfolio scoring.
* Increased the default `max_refinement_points` from 1,000 to 1,500 and retained
  the documented grid fallback for larger screened local searches.
* Added tests for screening upper bounds, feasible lower bounds, safe pruning,
  pair-intersection optima, and agreement between full and screened continuous
  searches.

The public API is unchanged from version 0.8.1. The preferred argument names
`n_hotspots` and `grid_spacing` remain in place, with the previously deprecated
aliases still supported through lifecycle warnings.

## Test environments

* local macOS Tahoe 26.5.2, R 4.6.1, aarch64

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies

There are currently no reverse dependencies on CRAN.
