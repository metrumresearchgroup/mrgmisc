#include <Rcpp.h>
using namespace Rcpp;
//' give the min value up to that point
//' @param x A numeric vector of values
//' @details
//' useful for safety analyses where an event may be defined as a certain change
//' in a biomarker, so need to see how the current measurement compares to the minimum
//' value up to that point
//' @examples \dontrun{
//'min_through(c(4, 3, 3, 2, 4, 1))
//'min_through(c(NA, 2))
//' }
//' @export
// [[Rcpp::export]]
NumericVector min_through(NumericVector x) {
  int n = x.size();
  NumericVector out = no_init(n);
  double min_val = INFINITY; //highest possible value to start so first non-NA value
  // will become the min value
  for (int i = 0; i < n; ++i) {
    if (R_IsNA(x[i])) {
      out[i] = NA_REAL;
      continue;
    } else { //not NA
      if(x[i] < min_val) {
        min_val = x[i];
      }
    }
    out[i]= min_val;
  }
  return out;
}

//' give the max value up to that point
//' @param x A numeric vector of values
//' @details
//' useful for safety analyses where an event may be defined as a certain change
//' in a biomarker, so need to see how the current measurement compares to the maximum
//' value up to that point
//' @examples \dontrun{
//'max_through(c(4, 3, 3, 2, 5, 1))
//'max_through(c(NA, 2, 1, 4, 2))
//' }
//' @export
// [[Rcpp::export]]
NumericVector max_through(NumericVector x) {
  int n = x.size();
  NumericVector out = no_init(n);
  double max_val = -INFINITY; //highest possible value to start so first non-NA value
  // will become the max value
  for (int i = 0; i < n; ++i) {
    if (R_IsNA(x[i])) {
      out[i] = NA_REAL;
      continue;
    } else { //not NA
      if(x[i] > max_val) {
        max_val = x[i];
      }
    }
    out[i]= max_val;
  }
  return out;
}