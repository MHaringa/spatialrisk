% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{update_rasterize}
\alias{update_rasterize}
\title{Update current rasterize for the next iteration}
\usage{
update_rasterize(old_rasterized, extent, new_spatvector, col)
}
\arguments{
\item{old_rasterized}{SpatRaster used in the current iteration.}

\item{extent}{Extent of the cells corresponding to the coordinates with the
highest concentration for the current iteration.Extent is output from
terra::ext().}

\item{new_spatvector}{Updated SpatVector for next iteration.}

\item{col}{Character. Variable name in \code{new_spatvector}.}
}
\description{
Update current rasterize for the next iteration.
}
\details{
Spatial extent refers to the geographic area covered by a spatial
dataset. It defines the boundaries in terms of its geographic coordinates
(north, east, south, west).
}
\author{
Martin Haringa
}
\keyword{internal}
