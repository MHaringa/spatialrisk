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






