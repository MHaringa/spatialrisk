## Resubmission
This is a resubmission. In this version I have:

* `highest_concentration()` is added to do a fast search for the coordinates of the highest concentration
* `neighborhood_gh_search()` is added to look for even higher concentrations in the neighborhood of the coordinates found by `highest_concentration()`

## Test environments
* local OS X install, R 4.0.5
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
I have also run R CMD check on downstream dependencies of spatialrisk.
All packages that I could install passed.


