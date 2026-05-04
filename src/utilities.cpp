#include <Rcpp.h>
#include <numeric>
#include <algorithm>
#include "geometry.h"
#include "utilities.h"

// [[Rcpp::export]]
double one_lon_in_meters(double lat0) {
  return one_longitude_degree_in_meters(lat0);
}

// [[Rcpp::export]]
Rcpp::NumericVector seq_cpp(double from, double to, int length_out) {

  if (length_out < 1) {
    Rcpp::stop("`length_out` must be at least 1.");
  }

  Rcpp::NumericVector out(length_out);

  if (length_out == 1) {
    out[0] = from;
    return out;
  }

  const double step =
    (to - from) / static_cast<double>(length_out - 1);

  for (int i = 0; i < length_out; ++i) {
    out[i] = from + i * step;
  }

  return out;
}

// [[Rcpp::export]]
Rcpp::IntegerVector highest_indices_cpp(Rcpp::NumericVector x, int n) {

  const int len = x.size();

  if (n < 1) {
    return Rcpp::IntegerVector(0);
  }

  n = std::min(n, len);

  Rcpp::IntegerVector ind(len);
  std::iota(ind.begin(), ind.end(), 0);

  std::partial_sort(
    ind.begin(),
    ind.begin() + n,
    ind.end(),
    [&](int i, int j) {
      if (Rcpp::NumericVector::is_na(x[i])) return false;
      if (Rcpp::NumericVector::is_na(x[j])) return true;
      return x[i] > x[j];
    }
  );

  ind.erase(ind.begin() + n, ind.end());

  return ind + 1;
}
