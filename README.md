# spatialrisk

spatialrisk is an R-package for spatial risk calculations. In particular, it can be used to determine concentration risk in the context of Solvency II. The package offers an effective approach to calculate the ‘standard formula’ under Solvency II. The ‘standard formula’ under Solvency II asks companies to report their largest fire concentration in respect of the fire peril within a radius of 200m. This is the maximum gross sum insured of the set of buildings fully or partly located within this radius. The package is currently work-in-progress and contributions are welcome.

The quickest way to get the development version of the package running locally is using devtools:

```r
devtools::install_github(repo = "MHaringa/spatialrisk")
library(spatialrisk)
```
