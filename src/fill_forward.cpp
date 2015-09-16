#include <Rcpp.h>
using namespace Rcpp;
//' given NA values fill them with the final non-na value
//' @param x A numeric vector of values
//' @details
//' Works very well in context of dplyr to carry out last-observation-carried-foward
//' for different individuals. It will NOT replace leading NA's
//' @examples \dontrun{
//' fill_forward(c(1.0, NA, 2))
//' fill_forward(c(NA, 1, NA, 2))
//' library(dplyr)
//' df <- data_frame(id = c(1, 1, 2, 2), obs = c(1.2, 4.8, 2.5, NA))
//' df %>% group_by(id) %>% mutate(obs_locf = fill_forward(obs))
//' }
//' @export
// [[Rcpp::export]]
NumericVector fill_forward(NumericVector x) {
  int n = x.size();
  NumericVector out = no_init(n);
  double stored_val = NA_REAL;
  for (int i = 0; i < n; ++i) {
    if (R_IsNA(x[i])) {
      if(stored_val == NA_REAL) {
        out[i] = NA_REAL;
      } else {
        out[i] = stored_val;
      }
    } else { //not NA
      out[i] = x[i];
      stored_val = x[i];
    }
  }
  return out;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
fill_forward(c(1.0, NA, 2))
fill_forward(c(NA, 1, NA, 2))
*/
