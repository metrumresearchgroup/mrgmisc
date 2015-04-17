#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

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
