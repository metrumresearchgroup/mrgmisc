#include <Rcpp.h>
using namespace Rcpp;
//' given a set of bin ranges, assign each value to a bin
//' @details
//' Given a set of quantiles/bins/etc established from a separate dataset, it can 
//' be useful to assign the same bins to new or simulated data for comparisons
//' or to do additional analysis such as assign dropouts etc. This function can be
//' used to take the breakpoints to establish bins quickly and easily
//' @param x A numeric vector of values
//' @param left,right Boundary values
//' @export
// [[Rcpp::export]]
IntegerVector set_bins_cpp(NumericVector x, NumericVector left, NumericVector right) {
  int n = x.size();
  int bin = left.size();
  IntegerVector out = no_init(n);
  
  for (int i = 0; i < n; ++i) {
    if (NumericVector::is_na(x[i])) {
      out[i] = NA_INTEGER;
    } else {
      // if not in range defined for bins assign NA value and go to next
      if (x[i] < left[0] || x[i] > right[bin-1]) {
        out[i] = NA_INTEGER;
      }
      for (int j = 0; j < bin; ++j) {
         if ( (x[i] >= left[j]) && (x[i] < right[j]) ) { 
          out[i] = j;
           break;
         } else {
           if (j == bin - 1) {
             if (x[i] == right[j]) {
             out[i] = j;  
             } else {
             out[i] = NA_INTEGER;
             }
           }
           continue;
          
         }
      }
    }
  }
  
  return out;
}

