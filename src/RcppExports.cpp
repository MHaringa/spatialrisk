// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cheap_dist_euclidean
double cheap_dist_euclidean(double lat1, double long1, double lat2, double long2);
RcppExport SEXP _spatialrisk_cheap_dist_euclidean(SEXP lat1SEXP, SEXP long1SEXP, SEXP lat2SEXP, SEXP long2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type lat1(lat1SEXP);
    Rcpp::traits::input_parameter< double >::type long1(long1SEXP);
    Rcpp::traits::input_parameter< double >::type lat2(lat2SEXP);
    Rcpp::traits::input_parameter< double >::type long2(long2SEXP);
    rcpp_result_gen = Rcpp::wrap(cheap_dist_euclidean(lat1, long1, lat2, long2));
    return rcpp_result_gen;
END_RCPP
}
// cheap_dist_euclidean_vec
Rcpp::NumericVector cheap_dist_euclidean_vec(Rcpp::NumericVector latFrom, Rcpp::NumericVector lonFrom, Rcpp::NumericVector latTo, Rcpp::NumericVector lonTo);
RcppExport SEXP _spatialrisk_cheap_dist_euclidean_vec(SEXP latFromSEXP, SEXP lonFromSEXP, SEXP latToSEXP, SEXP lonToSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type latFrom(latFromSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type lonFrom(lonFromSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type latTo(latToSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type lonTo(lonToSEXP);
    rcpp_result_gen = Rcpp::wrap(cheap_dist_euclidean_vec(latFrom, lonFrom, latTo, lonTo));
    return rcpp_result_gen;
END_RCPP
}
// haversine_cpp_vec
Rcpp::NumericVector haversine_cpp_vec(Rcpp::NumericVector latFrom, Rcpp::NumericVector lonFrom, Rcpp::NumericVector latTo, Rcpp::NumericVector lonTo, double earth_radius);
RcppExport SEXP _spatialrisk_haversine_cpp_vec(SEXP latFromSEXP, SEXP lonFromSEXP, SEXP latToSEXP, SEXP lonToSEXP, SEXP earth_radiusSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type latFrom(latFromSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type lonFrom(lonFromSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type latTo(latToSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type lonTo(lonToSEXP);
    Rcpp::traits::input_parameter< double >::type earth_radius(earth_radiusSEXP);
    rcpp_result_gen = Rcpp::wrap(haversine_cpp_vec(latFrom, lonFrom, latTo, lonTo, earth_radius));
    return rcpp_result_gen;
END_RCPP
}
// haversine_loop_cpp
DataFrame haversine_loop_cpp(DataFrame x, double lat_center, double lon_center, double radius);
RcppExport SEXP _spatialrisk_haversine_loop_cpp(SEXP xSEXP, SEXP lat_centerSEXP, SEXP lon_centerSEXP, SEXP radiusSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type lat_center(lat_centerSEXP);
    Rcpp::traits::input_parameter< double >::type lon_center(lon_centerSEXP);
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    rcpp_result_gen = Rcpp::wrap(haversine_loop_cpp(x, lat_center, lon_center, radius));
    return rcpp_result_gen;
END_RCPP
}
// concentration_loop_cpp
DataFrame concentration_loop_cpp(DataFrame sub, DataFrame ref, double radius, bool display_progress);
RcppExport SEXP _spatialrisk_concentration_loop_cpp(SEXP subSEXP, SEXP refSEXP, SEXP radiusSEXP, SEXP display_progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type sub(subSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type ref(refSEXP);
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< bool >::type display_progress(display_progressSEXP);
    rcpp_result_gen = Rcpp::wrap(concentration_loop_cpp(sub, ref, radius, display_progress));
    return rcpp_result_gen;
END_RCPP
}
// block_loop_cpp
DataFrame block_loop_cpp(DataFrame sub, DataFrame ref, double radius, bool display_progress);
RcppExport SEXP _spatialrisk_block_loop_cpp(SEXP subSEXP, SEXP refSEXP, SEXP radiusSEXP, SEXP display_progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type sub(subSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type ref(refSEXP);
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< bool >::type display_progress(display_progressSEXP);
    rcpp_result_gen = Rcpp::wrap(block_loop_cpp(sub, ref, radius, display_progress));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_spatialrisk_cheap_dist_euclidean", (DL_FUNC) &_spatialrisk_cheap_dist_euclidean, 4},
    {"_spatialrisk_cheap_dist_euclidean_vec", (DL_FUNC) &_spatialrisk_cheap_dist_euclidean_vec, 4},
    {"_spatialrisk_haversine_cpp_vec", (DL_FUNC) &_spatialrisk_haversine_cpp_vec, 5},
    {"_spatialrisk_haversine_loop_cpp", (DL_FUNC) &_spatialrisk_haversine_loop_cpp, 4},
    {"_spatialrisk_concentration_loop_cpp", (DL_FUNC) &_spatialrisk_concentration_loop_cpp, 4},
    {"_spatialrisk_block_loop_cpp", (DL_FUNC) &_spatialrisk_block_loop_cpp, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_spatialrisk(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
