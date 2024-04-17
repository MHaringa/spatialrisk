#include <Rcpp.h>
using namespace Rcpp;


double haversine_cpp(double lat1, double long1,
                     double lat2, double long2,
                     double earth_radius = 6378137){

  double distance;

  if (!((long1 > 360) || (long2 > 360) || (lat1 > 90) || (lat2 > 90))){
    double deg_to_rad = 0.0174532925199432957; // i.e. pi/180 (multiplication is faster than division)
    double delta_phi = (lat2 - lat1) * deg_to_rad;
    double delta_lambda = (long2 - long1) * deg_to_rad;
    double phi1 = lat1 * deg_to_rad;
    double phi2 = lat2 * deg_to_rad;
    double term1 = pow(sin(delta_phi * .5), 2);
    double term2 = cos(phi1) * cos(phi2) * pow(sin(delta_lambda * .5), 2);
    double delta_sigma = 2 * atan2(sqrt(term1 + term2), sqrt(1 - term1 - term2));
    distance = earth_radius * delta_sigma;
  } else {
    distance = NAN;
  }
  return distance;
}


// [[Rcpp::export]]
Rcpp::NumericVector haversine_cpp_vec(Rcpp::NumericVector latFrom, Rcpp::NumericVector lonFrom,
                                      Rcpp::NumericVector latTo, Rcpp::NumericVector lonTo,
                                      double earth_radius) {
  int n = latFrom.size();
  Rcpp::NumericVector distance(n);

  double latf;
  double latt;
  double lonf;
  double lont;
  double dist = 0;

  for(int i = 0; i < n; i++){

    latf = latFrom[i];
    lonf = lonFrom[i];
    latt = latTo[i];
    lont = lonTo[i];
    dist = haversine_cpp(latf, lonf, latt, lont, earth_radius);

    distance[i] = dist;
  }
  return distance;
}


// [[Rcpp::export]]
DataFrame haversine_loop_cpp(DataFrame x, double lat_center, double lon_center, double radius = 200) {

  // extracting each column into a vector
  IntegerVector id = seq(1, x.nrows());
  NumericVector lon = x["lon"];
  NumericVector lat = x["lat"];

  // create block around center point
  int circumference_earth_in_meters = 40075000;
  double one_lat_in_meters = circumference_earth_in_meters * 0.002777778;  // 0.002777778 is used instead of 1/360;
  double one_lon_in_meters = circumference_earth_in_meters * cos(lat_center * 0.01745329) * 0.002777778;
  double south_lat = lat_center - radius / one_lat_in_meters;
  double north_lat = lat_center + radius / one_lat_in_meters;
  double west_lon = lon_center - radius / one_lon_in_meters;
  double east_lon = lon_center + radius / one_lon_in_meters;

  // apply "pre-subsetting" before using haversine method
  int n = x.nrows();
  LogicalVector ind_block(n);

  for ( int i = 0; i < n; i++ ){
    ind_block[i] = !((lon[i] > east_lon) || (lon[i] < west_lon) || (lat[i] < south_lat) || (lat[i] > north_lat));
  }

  // create new data.frame based on "pre-subsetting"
  IntegerVector id_sub = id[ind_block];
  NumericVector lat_sub = lat[ind_block];
  NumericVector lon_sub = lon[ind_block];

  int n1 = id_sub.size();

  // apply haversine method to find points within radius from center
  NumericVector result(n1);
  for ( int i = 0; i < n1; ++i ) {
    result[i] = haversine_cpp(lat_center, lon_center, lat_sub[i], lon_sub[i]);
  }

  // create indicator whether coordinates are within radius
  LogicalVector ind_radius(n1);
  for (int i = 0; i < n1; i++){
    ind_radius[i] = (result[i] < radius);
  }

  // create a new data frame
  DataFrame NDF = DataFrame::create(Named("id") = id_sub[ind_radius],
                                    Named("distance_m") = result[ind_radius]);
  return(NDF);
}



// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
// [[Rcpp::export]]
DataFrame concentration_loop_cpp(DataFrame sub, DataFrame ref, double radius = 200, bool display_progress = true) {

  // extracting each column into a vector
  IntegerVector id_sub = seq(1, sub.nrows());
  NumericVector value_ref = ref["value"];
  NumericVector lon_sub = sub["lon"];
  NumericVector lat_sub = sub["lat"];
  NumericVector lon_ref = ref["lon"];
  NumericVector lat_ref = ref["lat"];

  // length of one latitude is the same everywhere
  int one_lat_in_meters = 111319;

  int n_sub = sub.nrows();
  NumericVector cumulation(n_sub);

  int n_ref = ref.nrows();
  LogicalVector ind_ref(n_ref);

  // determine cumulation per row
  Progress p(n_sub, display_progress);
  for ( int j = 0; j < n_sub; ++j ) {
    p.increment();

    // length of longitude depends on latitude
    double one_lon_in_meters = one_lat_in_meters * cos(lat_sub[j] * 0.01745329);

    // apply "pre-subsetting" before using haversine method
    for ( int i = 0; i < n_ref; i++ ){

      double dlat = fabs(lat_ref[i] - lat_sub[j]) * one_lat_in_meters;
      double dlon = fabs(lon_ref[i] - lon_sub[j]) * one_lon_in_meters;

      // check whether coordinates are in square
      if ( dlat > radius || dlon > radius) {
        ind_ref[i] = false;
      }

      // check whether coordinates are in diamond
      else if ( (dlon + dlat) < radius){
        ind_ref[i] = true;
      }

      // apply haversine for coordinates outside diamond and inside square
      else if ((haversine_cpp(lat_sub[j], lon_sub[j], lat_ref[i], lon_ref[i]) < radius)){
        ind_ref[i] = true;
      }

      else {
        ind_ref[i] = false;
      }
    }

    NumericVector value_id = value_ref[ind_ref];
    cumulation[j] = sum(value_id);
  }

  DataFrame NDF = DataFrame::create(Named("id") = id_sub,
                                    Named("cumulation") = cumulation);
  return(NDF);
}


// [[Rcpp::export]]
DataFrame block_loop_cpp(DataFrame sub, DataFrame ref, double radius = 200, bool display_progress = true) {

  // extracting each column into a vector
  IntegerVector id_sub = seq(1, sub.nrows());
  NumericVector value_ref = ref["value"];
  NumericVector lon_sub = sub["lon"];
  NumericVector lat_sub = sub["lat"];
  NumericVector lon_ref = ref["lon"];
  NumericVector lat_ref = ref["lat"];
  NumericVector delta_lon_sub = sub["delta_longitude"];
  NumericVector delta_lat_sub = sub["delta_latitude"];

  // length of one latitude is the same everywhere
  int one_lat_in_meters = 111319;

  int n_sub = sub.nrows();
  NumericVector cumulation(n_sub);

  int n_ref = ref.nrows();
  LogicalVector ind_ref(n_ref);

  // determine cumulation per row
  for ( int j = 0; j < n_sub; ++j ) {

    // length of longitude depends on latitude
    double one_lon_in_meters = one_lat_in_meters * cos(lat_sub[j] * 0.01745329);
    double south_lat = lat_sub[j] - delta_lat_sub[j] - radius / one_lat_in_meters;
    double north_lat = lat_sub[j] + delta_lat_sub[j] + radius / one_lat_in_meters;
    double west_lon = lon_sub[j] - delta_lon_sub[j] - radius / one_lon_in_meters;
    double east_lon = lon_sub[j] + delta_lon_sub[j] + radius / one_lon_in_meters;

    // apply "pre-subsetting" before using haversine method
    for ( int i = 0; i < n_ref; i++ ){

      if ( !((lon_ref[i] > east_lon) || (lon_ref[i] < west_lon) || (lat_ref[i] < south_lat) || (lat_ref[i] > north_lat)) ){
        ind_ref[i] = true;
      }

      else {
        ind_ref[i] = false;
      }
    }

    NumericVector value_id = value_ref[ind_ref];
    cumulation[j] = sum(value_id);
  }

  DataFrame NDF = DataFrame::create(Named("id") = id_sub,
                                    Named("cumulation") = cumulation);
  return(NDF);
}



// [[Rcpp::export]]
DataFrame concentration_loop_cpp2(DataFrame sub, DataFrame ref, double radius = 200) {

  // extracting each column into a vector
  IntegerVector id_sub = seq(1, sub.nrows());
  NumericVector value_ref = ref["value"];
  NumericVector x_sub = sub["x"];
  NumericVector y_sub = sub["y"];
  NumericVector x_ref = ref["x"];
  NumericVector y_ref = ref["y"];

  int n_sub = sub.nrows();
  NumericVector cumulation(n_sub);

  int n_ref = ref.nrows();
  LogicalVector ind_ref(n_ref);

  // determine cumulation per row
  for ( int j = 0; j < n_sub; ++j ) {

    // apply "pre-subsetting"
    for ( int i = 0; i < n_ref; i++ ){

      double dy = fabs(y_ref[i] - y_sub[j]);
      double dx = fabs(x_ref[i] - x_sub[j]);

      // check whether coordinates are in square
      if ( dy > radius || dx > radius) {
        ind_ref[i] = false;
      }

      // check whether coordinates are in diamond
      else if ( (dx + dy) < radius){
        ind_ref[i] = true;
      }

      // apply haversine for coordinates outside diamond and inside square
      else if ( sqrt(pow(dx, 2) + pow(dy, 2)) < radius ){
        ind_ref[i] = true;
      }

      else {
        ind_ref[i] = false;
      }
    }

    NumericVector value_id = value_ref[ind_ref];
    cumulation[j] = sum(value_id);
  }

  DataFrame NDF = DataFrame::create(Named("id") = id_sub,
                                    Named("cumulation") = cumulation);
  return(NDF);
}


#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
double one_lon_in_meters(double lat0) {
  double circumference_earth_in_meters = 40075000;
  return circumference_earth_in_meters * cos(lat0 * 0.01745329) * 0.002777778;
}

// [[Rcpp::export]]
DataFrame add_cell_bounds_cpp(DataFrame df, double size = 50, double radius = 200) {
  double circumference_earth_in_meters = 40075000;
  double one_lat_in_meters = circumference_earth_in_meters * 0.002777778;
  double dsize = size * .5;
  double cell_vert = dsize / one_lat_in_meters;
  double radd = radius + dsize;
  double overlap_vert = radd / one_lat_in_meters;

  NumericVector cell = df["cell"];
  NumericVector lat = df["lat"];
  NumericVector lon = df["lon"];

  int n_lat = lat.size();
  NumericVector cell_n(n_lat);
  NumericVector cell_s(n_lat);
  NumericVector cell_w(n_lat);
  NumericVector cell_e(n_lat);
  NumericVector over_n(n_lat);
  NumericVector over_s(n_lat);
  NumericVector over_w(n_lat);
  NumericVector over_e(n_lat);

  for (int i = 0; i < n_lat; i++){
    cell_n[i] = lat[i] + cell_vert;
    cell_s[i] = lat[i] - cell_vert;
    cell_w[i] = lon[i] - dsize / one_lon_in_meters(lat[i]);
    cell_e[i] = lon[i] + dsize / one_lon_in_meters(lat[i]);
    over_n[i] = lat[i] + overlap_vert;
    over_s[i] = lat[i] - overlap_vert;
    over_w[i] = lon[i] - radd / one_lon_in_meters(lat[i]);
    over_e[i] = lon[i] + radd / one_lon_in_meters(lat[i]);
  }

  DataFrame NDF = DataFrame::create(Named("cell") = cell,
                                    Named("lon") = lon,
                                    Named("lat") = lat,
                                    Named("cell_n") = cell_n,
                                    Named("cell_s") = cell_s,
                                    Named("cell_w") = cell_w,
                                    Named("cell_e") = cell_e,
                                    Named("over_n") = over_n,
                                    Named("over_s") = over_s,
                                    Named("over_w") = over_w,
                                    Named("over_e") = over_e);
  return(NDF);
}


// [[Rcpp::export]]
NumericVector seq_cpp(double from, double to, int length_out) {
  NumericVector seq_vec(length_out);
  double step = (to - from) / (length_out - 1);
  for (int i = 0; i < length_out; ++i) {
    seq_vec[i] = from + i * step;
  }
  return seq_vec;
}


// [[Rcpp::export]]
DataFrame expand_grid_cpp(NumericVector seq1, NumericVector seq2) {
  int len_seq1 = seq1.size();
  int len_seq2 = seq2.size();

  NumericVector Var1(len_seq1 * len_seq2);
  NumericVector Var2(len_seq1 * len_seq2);

  int counter = 0;
  for (int i = 0; i < len_seq1; ++i) {
    for (int j = 0; j < len_seq2; ++j) {
      Var1[counter] = seq1[i];
      Var2[counter] = seq2[j];
      counter++;
    }
  }

  return DataFrame::create(_["lon"] = Var1, _["lat"] = Var2);
}


// [[Rcpp::export]]
DataFrame filter_full_cpp(DataFrame dffull, DataFrame subb, int i) {

  NumericVector lon_dffull = dffull["lon"];
  NumericVector lat_dffull = dffull["lat"];
  NumericVector value_dffull = dffull["value"];
  NumericVector over_e = subb["over_e"];
  NumericVector over_w = subb["over_w"];
  NumericVector over_s = subb["over_s"];
  NumericVector over_n = subb["over_n"];

  int n_lon = lon_dffull.size();
  LogicalVector ind_lon(n_lon);

  for (int j = 0; j < n_lon; ++j) {
    if (lon_dffull[j] > over_e[i] ||
        lon_dffull[j] < over_w[i] ||
        lat_dffull[j] < over_s[i] ||
        lat_dffull[j] > over_n[i]) {
      ind_lon[j] = false;
    } else {
      ind_lon[j] = true;
    }
  }

  NumericVector lon_filt = lon_dffull[ind_lon];
  NumericVector lat_filt = lat_dffull[ind_lon];
  NumericVector value_filt = value_dffull[ind_lon];

  DataFrame NDF = DataFrame::create(Named("lon") = lon_filt,
                                    Named("lat") = lat_filt,
                                    Named("value") = value_filt);

  return(NDF);
}


// [[Rcpp::export]]
IntegerVector highest_indices_cpp(NumericVector x, int n) {
  int len = x.length();
  IntegerVector ind(len);
  std::iota(ind.begin(), ind.end(), 0);
  std::partial_sort(ind.begin(), ind.begin() + n, ind.end(),
                    [&](int i, int j) {
                      if(NumericVector::is_na(x[i])) return false;
                      if(NumericVector::is_na(x[j])) return true;
                      return x[i] > x[j]; });
  ind.erase(ind.begin() + n, ind.end());
  return(ind + 1);
}


// [[Rcpp::export]]
DataFrame max_conc_per_cell_cpp(DataFrame dfsub,
                                DataFrame dffull,
                                int points = 50,
                                int size = 50,
                                double radius = 200) {

  DataFrame subb = add_cell_bounds_cpp(dfsub, size, radius);
  NumericVector cell_w = subb["cell_w"];
  NumericVector cell_e = subb["cell_e"];
  NumericVector cell_s = subb["cell_s"];
  NumericVector cell_n = subb["cell_n"];
  NumericVector cell = subb["cell"];
  int n_subb = subb.nrows();

  // Define eg DataFrame outside the loop
  DataFrame eg;
  DataFrame ffull;
  DataFrame cloop;
  NumericVector id;
  NumericVector value_id;
  NumericVector conc;
  NumericVector value_conc;
  NumericVector res_conc(n_subb);
  NumericVector res_id(n_subb);
  NumericVector res_lon(n_subb);
  NumericVector res_lat(n_subb);


  for (int i = 0; i < n_subb; i++) {
    NumericVector x_seq = seq_cpp(cell_w[i], cell_e[i], points);
    NumericVector y_seq = seq_cpp(cell_s[i], cell_n[i], points);
    eg = expand_grid_cpp(x_seq, y_seq);
    ffull = filter_full_cpp(dffull, subb, i);
    cloop = concentration_loop_cpp(eg, ffull, radius, false);
    NumericVector concentration = cloop["cumulation"];
    int max_index = which_max(concentration);
    id = cloop["id"];
    conc = cloop["cumulation"];
    NumericVector eg_lon = eg["lon"];
    NumericVector eg_lat = eg["lat"];
    res_lon[i] = eg_lon[max_index];
    res_lat[i] = eg_lat[max_index];
    res_id[i] = id[max_index];
    res_conc[i] = conc[max_index];
  }

  // create a new data frame
  DataFrame NDF = DataFrame::create(Named("lon") = res_lon,
                                    Named("lat") = res_lat,
                                    Named("concentration") = res_conc,
                                    Named("cell") = cell);
  return(NDF);
}



