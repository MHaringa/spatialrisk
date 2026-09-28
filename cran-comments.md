## Submission

This is version 0.8.3 of spatialrisk, a minor release following 0.8.2.

The main changes are:

* Grid hotspot refinement and continuous-to-grid fallback now generate and
  score centres in `crs_metric`, using the same tolerance-aware closed-radius
  test as continuous search. Contributing records use that same projected
  test. Grid results can consequently differ from earlier longitude/latitude
  calculations; grid fallback remains an approximation, not a global optimum.
* Fixed continuous sweep preselection near radius boundaries. Conservative
  scoring intervals include the point scorer's distance slack without moving
  the geometric candidate centres. Near-equal angular events retain their own
  pair-derived coordinates, using the same construction as the full reference
  search. Accumulation guards account for sweep updates; signed weights bypass
  this upper-bound preselection.
* Protected indexed radius queries when the boundary tolerance crosses an
  index-cell edge. The additional cell is retrieved, but only points passing
  the existing distance test contribute. Added numerical-boundary, grid,
  fallback, and active-portfolio regression tests.

The public API is unchanged from version 0.8.2. 

## Test environments

* local macOS Tahoe 26.5.2, R 4.6.1, aarch64

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies

There are currently no reverse dependencies on CRAN.
