#include <Rcpp.h>
using namespace Rcpp;
//' given NA values fill them with the next non-na value
//' @param x A numeric vector of values
//' @details
//' Works very well in context of dplyr to carry out backwards imputation
//' @examples \dontrun{
//' fill_backward(c(1.0, NA, 2))
//' fill_backward(c(NA, 1, NA, 2))
//' library(dplyr)
//' df <- data_frame(id = c(1, 1, 2, 2), obs = c(1.2, 4.8, 2.5, NA))
//' df %>% group_by(id) %>% mutate(obs_imp = fill_backward(obs))
//' }
//' @export
// [[Rcpp::export]]
NumericVector fill_backward(NumericVector x) {
  int n = x.size();
  NumericVector out = NumericVector(n, NumericVector::get_na());
  for (int i = 0; i < n; ++i) {
    if (R_IsNA(x[i])) {
      for (int j = i+1; j < n; ++j) {
       if(R_IsNA(x[j])) {
         continue;
       } else {
         out[i] = x[j];
         break;
       } 
      }
    } else { //not NA
      out[i] = x[i];
    }
  }
  return out;
}


/*** R
fill_backward(c(1.0, NA, 2))
fill_backward(c(NA, 1.0, NA, 2))
fill_backward(c(NA, 1.0, NA, 2, NA, NA))
*/