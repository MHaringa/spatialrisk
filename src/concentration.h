#ifndef SPATIALRISK_CONCENTRATION_H
#define SPATIALRISK_CONCENTRATION_H

#include <Rcpp.h>

Rcpp::DataFrame concentration_loop_cpp(Rcpp::DataFrame sub,
                                       Rcpp::DataFrame ref,
                                       double radius,
                                       bool display_progress,
                                       double r);

#endif
