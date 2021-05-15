// // #include <Rcpp.h>
// using namespace Rcpp;
//
//
// // [[Rcpp::export]]
// double cheap_dist_euclidean(double lat1, double long1,
//                             double lat2, double long2){
//
//   double dy = 20004 * fabs(lat1 - lat2) * 0.005555556; // i.e. 1/180 (multiplication is faster than division)
//   double dx = 40008 * fabs(long1 - long2) * 0.002777778 * cos(.5 * (lat1 + lat2)); // i.e. 1/360
//   double d = sqrt(pow(dx, 2) + pow(dy, 2)) * 1000;
//   return d;
// }
//
//
// // [[Rcpp::export]]
// Rcpp::NumericVector cheap_dist_euclidean_vec(Rcpp::NumericVector latFrom,
//                                              Rcpp::NumericVector lonFrom,
//                                              Rcpp::NumericVector latTo,
//                                              Rcpp::NumericVector lonTo){
//   int n = latFrom.size();
//   Rcpp::NumericVector distance(n);
//
//   double latf;
//   double latt;
//   double lonf;
//   double lont;
//   double dist = 0;
//
//   for(int i = 0; i < n; i++){
//     latf = latFrom[i];
//     lonf = lonFrom[i];
//     latt = latTo[i];
//     lont = lonTo[i];
//     dist = cheap_dist_euclidean(latf, lonf, latt, lont);
//
//     distance[i] = dist;
//   }
//   return distance;
// }
//
//
//
