#ifndef SPATIALRISK_GEOMETRY_H
#define SPATIALRISK_GEOMETRY_H

#include <Rcpp.h>
#include <cmath>

inline double deg2rad(const double x) {
  static const double deg_to_rad = 0.01745329251994329576923690768489;
  return x * deg_to_rad;
}

inline double one_latitude_degree_in_meters() {
  static const double meters = 111319.44444444444;
  return meters;
}

inline double one_longitude_degree_in_meters(const double lat) {
  return one_latitude_degree_in_meters() * std::cos(deg2rad(lat));
}

inline double haversine_distance(const double lat_from,
                                 const double lon_from,
                                 const double lat_to,
                                 const double lon_to,
                                 const double r) {

  if (Rcpp::NumericVector::is_na(lat_from) ||
      Rcpp::NumericVector::is_na(lon_from) ||
      Rcpp::NumericVector::is_na(lat_to) ||
      Rcpp::NumericVector::is_na(lon_to)) {
    return NA_REAL;
  }

  const double lat_from_rad = deg2rad(lat_from);
  const double lat_to_rad = deg2rad(lat_to);

  const double half_delta_lat = deg2rad(lat_to - lat_from) * 0.5;
  const double half_delta_lon = deg2rad(lon_to - lon_from) * 0.5;
  const double sin_half_delta_lat = std::sin(half_delta_lat);
  const double sin_half_delta_lon = std::sin(half_delta_lon);

  double a =
    sin_half_delta_lat * sin_half_delta_lat +
    std::cos(lat_from_rad) *
    std::cos(lat_to_rad) *
    sin_half_delta_lon * sin_half_delta_lon;

  if (a > 1.0) {
    a = 1.0;
  }

  const double c =
    2.0 * std::atan2(std::sqrt(a), std::sqrt(1.0 - a));

  return r * c;
}

#endif
