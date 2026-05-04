#include <Rcpp.h>
#include "geometry.h"

// [[Rcpp::export]]
Rcpp::DataFrame points_in_radius_cpp(Rcpp::DataFrame x,
                                     double lat_center,
                                     double lon_center,
                                     double radius = 200,
                                     double r = 6378137.0) {

  if (!std::isfinite(radius) || radius <= 0.0) {
    Rcpp::stop("`radius` must be a single finite positive numeric value.");
  }

  Rcpp::NumericVector lon = x["lon"];
  Rcpp::NumericVector lat = x["lat"];

  const int n = x.nrows();

  std::vector<int> ids;
  std::vector<double> distances;

  ids.reserve(n);
  distances.reserve(n);

  if (Rcpp::NumericVector::is_na(lat_center) ||
      Rcpp::NumericVector::is_na(lon_center)) {
    return Rcpp::DataFrame::create(
      Rcpp::Named("id") = ids,
      Rcpp::Named("distance_m") = distances
    );
  }

  const double one_lat_m = one_latitude_degree_in_meters();
  const double one_lon_m = one_longitude_degree_in_meters(lat_center);

  const double south_lat = lat_center - radius / one_lat_m;
  const double north_lat = lat_center + radius / one_lat_m;
  const double west_lon = lon_center - radius / one_lon_m;
  const double east_lon = lon_center + radius / one_lon_m;

  for (int i = 0; i < n; ++i) {

    if (Rcpp::NumericVector::is_na(lat[i]) ||
        Rcpp::NumericVector::is_na(lon[i])) {
      continue;
    }

    if (lon[i] < west_lon || lon[i] > east_lon ||
        lat[i] < south_lat || lat[i] > north_lat) {
      continue;
    }

    const double distance = haversine_distance(
      lat_center,
      lon_center,
      lat[i],
         lon[i],
            r
    );

    if (!Rcpp::NumericVector::is_na(distance) &&
        distance < radius) {
      ids.push_back(i + 1);
      distances.push_back(distance);
    }
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("id") = ids,
    Rcpp::Named("distance_m") = distances
  );
}

// [[Rcpp::export]]
Rcpp::DataFrame points_in_radius_multi_cpp(Rcpp::DataFrame x,
                                           Rcpp::NumericVector lat_centers,
                                           Rcpp::NumericVector lon_centers,
                                           double radius = 200,
                                           double r = 6378137.0) {

  if (lat_centers.size() != lon_centers.size()) {
    Rcpp::stop("`lat_centers` and `lon_centers` must have the same length.");
  }

  if (!std::isfinite(radius) || radius <= 0.0) {
    Rcpp::stop("`radius` must be a single finite positive numeric value.");
  }

  Rcpp::NumericVector lon = x["lon"];
  Rcpp::NumericVector lat = x["lat"];

  const int n = x.nrows();
  const int n_centers = lat_centers.size();

  std::vector<int> result_id;
  std::vector<double> result_distance_m;
  std::vector<int> result_center_index;

  result_id.reserve(n);
  result_distance_m.reserve(n);
  result_center_index.reserve(n);

  const double one_lat_m = one_latitude_degree_in_meters();

  for (int j = 0; j < n_centers; ++j) {

    const double lat_center = lat_centers[j];
    const double lon_center = lon_centers[j];

    if (Rcpp::NumericVector::is_na(lat_center) ||
        Rcpp::NumericVector::is_na(lon_center)) {
      continue;
    }

    const double one_lon_m =
      one_longitude_degree_in_meters(lat_center);

    const double south_lat = lat_center - radius / one_lat_m;
    const double north_lat = lat_center + radius / one_lat_m;
    const double west_lon = lon_center - radius / one_lon_m;
    const double east_lon = lon_center + radius / one_lon_m;

    for (int i = 0; i < n; ++i) {

      if (Rcpp::NumericVector::is_na(lat[i]) ||
          Rcpp::NumericVector::is_na(lon[i])) {
        continue;
      }

      if (lon[i] < west_lon || lon[i] > east_lon ||
          lat[i] < south_lat || lat[i] > north_lat) {
        continue;
      }

      const double distance = haversine_distance(
        lat_center,
        lon_center,
        lat[i],
           lon[i],
              r
      );

      if (!Rcpp::NumericVector::is_na(distance) &&
          distance < radius) {
        result_id.push_back(i + 1);
        result_distance_m.push_back(distance);
        result_center_index.push_back(j + 1);
      }
    }
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("id") = result_id,
    Rcpp::Named("distance_m") = result_distance_m,
    Rcpp::Named("center_index") = result_center_index
  );
}

// Compatibility wrapper.
// [[Rcpp::export]]
Rcpp::DataFrame haversine_loop_cpp0(Rcpp::DataFrame x,
                                    Rcpp::NumericVector lat_centers,
                                    Rcpp::NumericVector lon_centers,
                                    double radius = 200) {
  return points_in_radius_multi_cpp(
    x,
    lat_centers,
    lon_centers,
    radius,
    6378137.0
  );
}
