// [[Rcpp::depends(RcppProgress)]]
#include <Rcpp.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "geometry.h"
#include "concentration.h"

// [[Rcpp::export]]
Rcpp::DataFrame concentration_loop_cpp(Rcpp::DataFrame sub,
                                       Rcpp::DataFrame ref,
                                       double radius = 200,
                                       bool display_progress = true,
                                       double r = 6378137.0) {

  if (!std::isfinite(radius) || radius <= 0.0) {
    Rcpp::stop("`radius` must be a single finite positive numeric value.");
  }

  Rcpp::NumericVector lon_sub = sub["lon"];
  Rcpp::NumericVector lat_sub = sub["lat"];

  Rcpp::NumericVector lon_ref = ref["lon"];
  Rcpp::NumericVector lat_ref = ref["lat"];
  Rcpp::NumericVector value_ref = ref["value"];

  const int n_sub = sub.nrows();
  const int n_ref = ref.nrows();

  Rcpp::NumericVector cumulation(n_sub);

  const double one_lat_m = one_latitude_degree_in_meters();

  std::vector<double> lon_ref_valid;
  std::vector<double> lat_ref_valid;
  std::vector<double> value_ref_valid;

  lon_ref_valid.reserve(n_ref);
  lat_ref_valid.reserve(n_ref);
  value_ref_valid.reserve(n_ref);

  for (int i = 0; i < n_ref; ++i) {
    if (Rcpp::NumericVector::is_na(lat_ref[i]) ||
        Rcpp::NumericVector::is_na(lon_ref[i]) ||
        Rcpp::NumericVector::is_na(value_ref[i])) {
      continue;
    }

    lon_ref_valid.push_back(lon_ref[i]);
    lat_ref_valid.push_back(lat_ref[i]);
    value_ref_valid.push_back(value_ref[i]);
  }

  const int n_ref_valid = lon_ref_valid.size();

  Progress p(n_sub, display_progress);

  for (int j = 0; j < n_sub; ++j) {

    if (Progress::check_abort()) {
      return Rcpp::DataFrame::create(
        Rcpp::Named("id") = Rcpp::seq(1, n_sub),
        Rcpp::Named("cumulation") = cumulation
      );
    }

    p.increment();

    if (Rcpp::NumericVector::is_na(lat_sub[j]) ||
        Rcpp::NumericVector::is_na(lon_sub[j])) {
      cumulation[j] = NA_REAL;
      continue;
    }

    const double one_lon_m =
      one_longitude_degree_in_meters(lat_sub[j]);

    double total = 0.0;

    for (int i = 0; i < n_ref_valid; ++i) {

      const double dlat =
        std::fabs(lat_ref_valid[i] - lat_sub[j]) * one_lat_m;

      const double dlon =
        std::fabs(lon_ref_valid[i] - lon_sub[j]) * one_lon_m;

      if (dlat > radius || dlon > radius) {
        continue;
      }

      if ((dlat + dlon) < radius) {
        total += value_ref_valid[i];
        continue;
      }

      const double distance = haversine_distance(
        lat_sub[j],
               lon_sub[j],
                      lat_ref_valid[i],
                             lon_ref_valid[i],
                                    r
      );

      if (!Rcpp::NumericVector::is_na(distance) &&
          distance < radius) {
        total += value_ref_valid[i];
      }
    }

    cumulation[j] = total;
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("id") = Rcpp::seq(1, n_sub),
    Rcpp::Named("cumulation") = cumulation
  );
}

// [[Rcpp::export]]
Rcpp::DataFrame block_loop_cpp(Rcpp::DataFrame sub,
                               Rcpp::DataFrame ref,
                               double radius = 200) {

  Rcpp::NumericVector lon_sub = sub["lon"];
  Rcpp::NumericVector lat_sub = sub["lat"];
  Rcpp::NumericVector delta_lon_sub = sub["delta_longitude"];
  Rcpp::NumericVector delta_lat_sub = sub["delta_latitude"];

  Rcpp::NumericVector lon_ref = ref["lon"];
  Rcpp::NumericVector lat_ref = ref["lat"];
  Rcpp::NumericVector value_ref = ref["value"];

  const int n_sub = sub.nrows();
  const int n_ref = ref.nrows();

  Rcpp::NumericVector cumulation(n_sub);

  const double one_lat_m = one_latitude_degree_in_meters();

  for (int j = 0; j < n_sub; ++j) {

    if (Rcpp::NumericVector::is_na(lat_sub[j]) ||
        Rcpp::NumericVector::is_na(lon_sub[j]) ||
        Rcpp::NumericVector::is_na(delta_lat_sub[j]) ||
        Rcpp::NumericVector::is_na(delta_lon_sub[j])) {
      cumulation[j] = NA_REAL;
      continue;
    }

    const double one_lon_m =
      one_longitude_degree_in_meters(lat_sub[j]);

    const double south_lat =
      lat_sub[j] - delta_lat_sub[j] - radius / one_lat_m;

    const double north_lat =
      lat_sub[j] + delta_lat_sub[j] + radius / one_lat_m;

    const double west_lon =
      lon_sub[j] - delta_lon_sub[j] - radius / one_lon_m;

    const double east_lon =
      lon_sub[j] + delta_lon_sub[j] + radius / one_lon_m;

    double total = 0.0;

    for (int i = 0; i < n_ref; ++i) {

      if (Rcpp::NumericVector::is_na(lat_ref[i]) ||
          Rcpp::NumericVector::is_na(lon_ref[i]) ||
          Rcpp::NumericVector::is_na(value_ref[i])) {
        continue;
      }

      if (lon_ref[i] >= west_lon &&
          lon_ref[i] <= east_lon &&
          lat_ref[i] >= south_lat &&
          lat_ref[i] <= north_lat) {
        total += value_ref[i];
      }
    }

    cumulation[j] = total;
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("id") = Rcpp::seq(1, n_sub),
    Rcpp::Named("cumulation") = cumulation
  );
}

// [[Rcpp::export]]
Rcpp::DataFrame concentration_loop_xy_cpp(Rcpp::DataFrame sub,
                                          Rcpp::DataFrame ref,
                                          double radius = 200) {

  Rcpp::NumericVector x_sub = sub["x"];
  Rcpp::NumericVector y_sub = sub["y"];

  Rcpp::NumericVector x_ref = ref["x"];
  Rcpp::NumericVector y_ref = ref["y"];
  Rcpp::NumericVector value_ref = ref["value"];

  const int n_sub = sub.nrows();
  const int n_ref = ref.nrows();

  Rcpp::NumericVector cumulation(n_sub);

  const double radius_squared = radius * radius;

  for (int j = 0; j < n_sub; ++j) {

    if (Rcpp::NumericVector::is_na(x_sub[j]) ||
        Rcpp::NumericVector::is_na(y_sub[j])) {
      cumulation[j] = NA_REAL;
      continue;
    }

    double total = 0.0;

    for (int i = 0; i < n_ref; ++i) {

      if (Rcpp::NumericVector::is_na(x_ref[i]) ||
          Rcpp::NumericVector::is_na(y_ref[i]) ||
          Rcpp::NumericVector::is_na(value_ref[i])) {
        continue;
      }

      const double dx = x_ref[i] - x_sub[j];
      const double dy = y_ref[i] - y_sub[j];

      if (std::fabs(dx) > radius || std::fabs(dy) > radius) {
        continue;
      }

      if (dx * dx + dy * dy < radius_squared) {
        total += value_ref[i];
      }
    }

    cumulation[j] = total;
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("id") = Rcpp::seq(1, n_sub),
    Rcpp::Named("cumulation") = cumulation
  );
}

// Compatibility wrapper.
// [[Rcpp::export]]
Rcpp::DataFrame concentration_loop_cpp2(Rcpp::DataFrame sub,
                                        Rcpp::DataFrame ref,
                                        double radius = 200) {
  return concentration_loop_xy_cpp(sub, ref, radius);
}
