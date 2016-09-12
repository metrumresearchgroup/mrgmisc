#include <Rcpp.h>
using namespace Rcpp;
//' given NA values fill them with the next non-na value
//' @param x A numeric vector of values
//' @details
//' Works very well in context of dplyr to carry out backwards imputation
//' @examples 
//' fill_backward(c(1.0, NA, 2))
//' fill_backward(c(NA, 1, NA, 2))
//' library(dplyr)
//' df <- data_frame(id = c(1, 1, 2, 2), obs = c(1.2, 4.8, 2.5, NA))
//' df %>% group_by(id) %>% mutate(obs_imp = fill_backward(obs))
//' @export
// [[Rcpp::export]]
NumericVector fill_backward(NumericVector x) {
  int n = x.size();
  double stored_val = NA_REAL;
  for (int i = n-1; i >= 0; --i) {
    if (R_IsNA(x[i])) {
        x[i] = stored_val;
    } else { //not NA
      stored_val = x[i];
    }
  }
  return x;
}


/*** R
fill_backward(c(1.0, NA, 2))
fill_backward(c(NA, 1.0, NA, 2))
fill_backward(c(NA, 1.0, NA, 2, NA, NA))
*/