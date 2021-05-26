## Resubmission
This is a resubmission. In this version I have:

* `highest_concentration()` now returns correct highest concentration when the circle of the highest concentration overlaps more than one geohash (bug). 
* `plot.concentration()` now handles many lon/lat pairs better

## Test environments
* local OS X install, R 4.0.5
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
I have also run R CMD check on downstream dependencies of spatialrisk.
All packages that I could install passed.


