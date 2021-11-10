## Resubmission
This is a resubmission. In this version I have:

* `neighborhood_gh_search()` now returns a more precise outcome when the radius of the circle is not equal to 200m
* `sf::st_crs()` is used for `sf` objects to not show the message that old crs is detected anymore

## Test environments
* local OS X install, R 4.1.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
I have also run R CMD check on downstream dependencies of spatialrisk.
All packages that I could install passed.


