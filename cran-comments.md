## Resubmission
This is a resubmission. In this version I have:

* Introduced `find_highest_concentration()`: a faster and more accurate alternative to `highest_concentration()`, leveraging focal statistics for optimal results.
* Deprecated `highest_concentration()` in favor of the new, improved function.
* Updated `plot_points()` to utilize `mapview::mapview()` for enhanced interactive map visualizations.
* Revised the README to reflect these new features and updates.

## Test environments
* local OS X install, R 4.5.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
I have also run R CMD check on downstream dependencies of spatialrisk.
All packages that I could install passed.


