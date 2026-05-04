#include <Rcpp.h>
#include "geometry.h"

// [[Rcpp::export]]
Rcpp::NumericVector haversine_cpp_vec(Rcpp::NumericVector lat_from,
                                      Rcpp::NumericVector lon_from,
                                      Rcpp::NumericVector lat_to,
                                      Rcpp::NumericVector lon_to,
                                      double r) {

  const int n = lat_from.size();

  if (lon_from.size() != n ||
      lat_to.size() != n ||
      lon_to.size() != n) {
    Rcpp::stop(
      "`lat_from`, `lon_from`, `lat_to`, and `lon_to` must have the same length."
    );
  }

  Rcpp::NumericVector out(n);

  for (int i = 0; i < n; ++i) {
    out[i] = haversine_distance(
      lat_from[i],
      lon_from[i],
      lat_to[i],
      lon_to[i],
      r
    );
  }

  return out;
}

// Optional scalar wrapper for R use.
// [[Rcpp::export]]
double haversine_cpp(double lat_from,
                     double lon_from,
                     double lat_to,
                     double lon_to,
                     double r = 6378137.0) {

  return haversine_distance(
    lat_from,
    lon_from,
    lat_to,
    lon_to,
    r
  );
}
