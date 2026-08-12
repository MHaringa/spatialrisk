## Submission

This is version 0.8.1 of spatialrisk. It is a patch release after 0.8.0.

In this version I have:

* Added a decomposed hotspot workflow with `prepare_spatialrisk()`,
  `select_candidates()`, and `optimize_hotspot()`. These functions expose the
  preparation, candidate-selection, and optimisation steps used by
  `concentration_hotspot()`, while keeping `concentration_hotspot()` as the
  main wrapper for the complete workflow.
* Improved `concentration_hotspot(method = "continuous")` and
  `optimize_hotspot()` so pair-intersection refinement is evaluated over all
  focal candidate cells above the lower bound, rather than only around the top
  focal cell.
* Updated the continuous hotspot workflow so candidate centres are scored
  against the full remaining portfolio before the best hotspot is selected.
  This avoids cases where a later `top_n` hotspot could have a higher
  concentration than the first reported hotspot.
* Improved `top_n > 1` performance for the continuous hotspot method by caching
  pair-intersection refinements per focal candidate cell and recomputing only
  cache entries affected by removed contributing points or changed focal cells.
* Added tests for the decomposed hotspot workflow and a regression test checking
  that continuous `top_n` hotspot concentrations are non-increasing after
  contributing points are removed between iterations.
* Separated deprecated compatibility wrappers into their own Rd files so pkgdown
  no longer merges deprecated aliases with the current function documentation.

## Test environments

* local OS X install, R 4.5.3
* win-builder (devel and release)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

I have also run R CMD check on downstream dependencies of spatialrisk.
All packages that I could install passed.
