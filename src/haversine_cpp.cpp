#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double haversine_cpp(double lat1, double long1,
                     double lat2, double long2,
                     double earth_radius = 6378137){
  double deg_to_rad = 0.0174532925199432957; // i.e. pi/180 (multiplication is faster than division)
  double delta_phi = (lat2 - lat1) * deg_to_rad;
  double delta_lambda = (long2 - long1) * deg_to_rad;
  double phi1 = lat1 * deg_to_rad;
  double phi2 = lat2 * deg_to_rad;
  double term1 = pow(sin(delta_phi * .5), 2);
  double term2 = cos(phi1) * cos(phi2) * pow(sin(delta_lambda * .5), 2);
  double delta_sigma = 2 * atan2(sqrt(term1 + term2), sqrt(1 - term1 - term2));
  double distance = earth_radius * delta_sigma;
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
  double south_lat = lat_center - (radius + 2) / one_lat_in_meters;
  double north_lat = lat_center + (radius + 2)  / one_lat_in_meters;
  double west_lon = lon_center - (radius + 2)  / one_lon_in_meters;
  double east_lon = lon_center + (radius + 2)  / one_lon_in_meters;

  // apply "pre-subsetting" before using haversine method
  int n = x.nrows();
  LogicalVector ind_block(n);

  for ( int i = 0; i < n; i++ ){
    ind_block[i] = (lon[i] < east_lon & lon[i] > west_lon & lat[i] > south_lat & lat[i] < north_lat);
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
                                    Named("lon") = lon_sub[ind_radius],
                                    Named("lat") = lat_sub[ind_radius],
                                    Named("distance_m") = result[ind_radius]);
  return(NDF);
}



// [[Rcpp::export]]
DataFrame concentration_loop_cpp(DataFrame sub, DataFrame ref, double radius = 200) {

  // extracting each column into a vector
  IntegerVector id = seq(1, sub.nrows());
  NumericVector amount = sub["amount"];
  NumericVector lon = sub["lon"];
  NumericVector lat = sub["lat"];

  // define length of loop and create output vector
  int n = sub.nrows();
  NumericVector cumulation(n);

  // determine cumulation per row
  for ( int i = 0; i < n; ++i ) {
    DataFrame result = haversine_loop_cpp(ref, lat[i], lon[i], radius);
    NumericVector amount = result["amount"];
    cumulation[i] = sum(amount);
  }

  // create a new data frame
  DataFrame NDF = DataFrame::create(Named("id") = id,
                                    Named("amount") = amount,
                                    Named("lon") = lon,
                                    Named("lat") = lat,
                                    Named("cumulation") = cumulation);
  return(NDF);
}






