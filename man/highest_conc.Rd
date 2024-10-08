% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{highest_conc}
\alias{highest_conc}
\title{Find the highest concentration for the current iteration}
\usage{
highest_conc(hf_conc_new, high_foc, db)
}
\arguments{
\item{hf_conc_new}{highest concentrations from the current iteration,
retrieved from \code{conc_per_cell_new()}.}

\item{high_foc}{data.frame containing cell ids with the top n focal values
from the current iteration.}

\item{db}{data.frame containing previously saved highest concentrations.}
}
\description{
Find the highest concentration for the current iteration.
}
\author{
Martin Haringa
}
\keyword{internal}
