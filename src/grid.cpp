#include <Rcpp.h>
#include "geometry.h"
#include "utilities.h"
#include "concentration.h"

// [[Rcpp::export]]
Rcpp::DataFrame add_cell_bounds_cpp(Rcpp::DataFrame df,
                                    double size = 50,
                                    double radius = 200) {

  Rcpp::NumericVector cell = df["cell"];
  Rcpp::NumericVector lat = df["lat"];
  Rcpp::NumericVector lon = df["lon"];

  const int n = lat.size();

  Rcpp::NumericVector cell_n(n);
  Rcpp::NumericVector cell_s(n);
  Rcpp::NumericVector cell_w(n);
  Rcpp::NumericVector cell_e(n);

  Rcpp::NumericVector over_n(n);
  Rcpp::NumericVector over_s(n);
  Rcpp::NumericVector over_w(n);
  Rcpp::NumericVector over_e(n);

  const double half_size = size * 0.5;
  const double radius_plus_half_size = radius + half_size;
  const double one_lat_m = one_latitude_degree_in_meters();

  for (int i = 0; i < n; ++i) {

    if (Rcpp::NumericVector::is_na(lat[i]) ||
        Rcpp::NumericVector::is_na(lon[i])) {
      cell_n[i] = NA_REAL;
      cell_s[i] = NA_REAL;
      cell_w[i] = NA_REAL;
      cell_e[i] = NA_REAL;
      over_n[i] = NA_REAL;
      over_s[i] = NA_REAL;
      over_w[i] = NA_REAL;
      over_e[i] = NA_REAL;
      continue;
    }

    const double one_lon_m =
      one_longitude_degree_in_meters(lat[i]);

    cell_n[i] = lat[i] + half_size / one_lat_m;
    cell_s[i] = lat[i] - half_size / one_lat_m;
    cell_w[i] = lon[i] - half_size / one_lon_m;
    cell_e[i] = lon[i] + half_size / one_lon_m;

    over_n[i] = lat[i] + radius_plus_half_size / one_lat_m;
    over_s[i] = lat[i] - radius_plus_half_size / one_lat_m;
    over_w[i] = lon[i] - radius_plus_half_size / one_lon_m;
    over_e[i] = lon[i] + radius_plus_half_size / one_lon_m;
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("cell") = cell,
    Rcpp::Named("lon") = lon,
    Rcpp::Named("lat") = lat,
    Rcpp::Named("cell_n") = cell_n,
    Rcpp::Named("cell_s") = cell_s,
    Rcpp::Named("cell_w") = cell_w,
    Rcpp::Named("cell_e") = cell_e,
    Rcpp::Named("over_n") = over_n,
    Rcpp::Named("over_s") = over_s,
    Rcpp::Named("over_w") = over_w,
    Rcpp::Named("over_e") = over_e
  );
}

// [[Rcpp::export]]
Rcpp::DataFrame expand_grid_cpp(Rcpp::NumericVector seq1,
                                Rcpp::NumericVector seq2) {

  const int n1 = seq1.size();
  const int n2 = seq2.size();

  Rcpp::NumericVector lon(n1 * n2);
  Rcpp::NumericVector lat(n1 * n2);

  int counter = 0;

  for (int i = 0; i < n1; ++i) {
    for (int j = 0; j < n2; ++j) {
      lon[counter] = seq1[i];
      lat[counter] = seq2[j];
      ++counter;
    }
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("lon") = lon,
    Rcpp::Named("lat") = lat
  );
}

// [[Rcpp::export]]
Rcpp::DataFrame filter_full_cpp(Rcpp::DataFrame dffull,
                                Rcpp::DataFrame subb,
                                int i) {

  const int idx = i - 1;

  Rcpp::NumericVector lon = dffull["lon"];
  Rcpp::NumericVector lat = dffull["lat"];
  Rcpp::NumericVector value = dffull["value"];

  Rcpp::NumericVector over_e = subb["over_e"];
  Rcpp::NumericVector over_w = subb["over_w"];
  Rcpp::NumericVector over_s = subb["over_s"];
  Rcpp::NumericVector over_n = subb["over_n"];

  std::vector<double> lon_out;
  std::vector<double> lat_out;
  std::vector<double> value_out;

  const int n = lon.size();

  lon_out.reserve(n);
  lat_out.reserve(n);
  value_out.reserve(n);

  for (int j = 0; j < n; ++j) {

    if (Rcpp::NumericVector::is_na(lon[j]) ||
        Rcpp::NumericVector::is_na(lat[j]) ||
        Rcpp::NumericVector::is_na(value[j])) {
      continue;
    }

    if (lon[j] >= over_w[idx] &&
        lon[j] <= over_e[idx] &&
        lat[j] >= over_s[idx] &&
        lat[j] <= over_n[idx]) {

      lon_out.push_back(lon[j]);
      lat_out.push_back(lat[j]);
      value_out.push_back(value[j]);
    }
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("lon") = lon_out,
    Rcpp::Named("lat") = lat_out,
    Rcpp::Named("value") = value_out
  );
}

// [[Rcpp::export]]
Rcpp::DataFrame max_conc_per_cell_cpp(Rcpp::DataFrame dfsub,
                                      Rcpp::DataFrame dffull,
                                      int points = 50,
                                      double size = 50,
                                      double radius = 200,
                                      double r = 6378137.0) {

  Rcpp::DataFrame subb =
    add_cell_bounds_cpp(dfsub, size, radius);

  Rcpp::NumericVector cell_w = subb["cell_w"];
  Rcpp::NumericVector cell_e = subb["cell_e"];
  Rcpp::NumericVector cell_s = subb["cell_s"];
  Rcpp::NumericVector cell_n = subb["cell_n"];
  Rcpp::NumericVector cell = subb["cell"];

  const int n = subb.nrows();

  Rcpp::NumericVector res_lon(n);
  Rcpp::NumericVector res_lat(n);
  Rcpp::NumericVector res_conc(n);

  for (int i = 0; i < n; ++i) {

    Rcpp::NumericVector x_seq =
      seq_cpp(cell_w[i], cell_e[i], points);

    Rcpp::NumericVector y_seq =
      seq_cpp(cell_s[i], cell_n[i], points);

    Rcpp::DataFrame grid =
      expand_grid_cpp(x_seq, y_seq);

    Rcpp::DataFrame ref_filtered =
      filter_full_cpp(dffull, subb, i + 1);

    Rcpp::DataFrame conc_df =
      concentration_loop_cpp(
        grid,
        ref_filtered,
        radius,
        false,
        r
      );

    Rcpp::NumericVector concentration =
      conc_df["cumulation"];

    const int max_index =
      Rcpp::which_max(concentration);

    Rcpp::NumericVector grid_lon = grid["lon"];
    Rcpp::NumericVector grid_lat = grid["lat"];

    res_lon[i] = grid_lon[max_index];
    res_lat[i] = grid_lat[max_index];
    res_conc[i] = concentration[max_index];
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("lon") = res_lon,
    Rcpp::Named("lat") = res_lat,
    Rcpp::Named("concentration") = res_conc,
    Rcpp::Named("cell") = cell
  );
}
