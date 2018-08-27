
#' Concentration cpp
#'
#' @param sub deelverzameling
#' @param ref origineel
#'
#' @return df
#' @export concentration_cpp
#'
#' @useDynLib spatialrisk
#' @importFrom Rcpp sourceCpp
#'
#'
#' @examples
concentration_cpp <- function(sub, ref){

  concentration_loop_cpp(sub, ref, radius = 200)

}
