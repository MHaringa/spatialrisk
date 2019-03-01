## Resubmission
This is a resubmission. In this version I have:

* Rewritten C++ code. Equation for testing if a point is inside a circle is extended with testing whether point is inside a square diamond inside the square before applying the Haversine formula. This resulted in a speed enhancement (approx. ~2x faster). 

## Test environments
* local OS X install, R 3.5.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
I have also run R CMD check on downstream dependencies of spatialrisk.
All packages that I could install passed.


