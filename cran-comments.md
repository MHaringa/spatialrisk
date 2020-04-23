## Resubmission
This is a resubmission. In this version I have:

* A package website is added using pkgdown.
* `concentration()` and `points_in_circle()` now return error messages when the data does not contain columns for `lon` and `lat`.
* `concentration()` and `points_in_circle()` have updated documentation.
* `haversine()` now returns NA when coordinates are missing.

## Test environments
* local OS X install, R 3.6.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
I have also run R CMD check on downstream dependencies of spatialrisk.
All packages that I could install passed.


