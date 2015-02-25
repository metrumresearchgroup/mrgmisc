#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)
//
// Learn more about how to use Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//
// and browse examples of code using Rcpp at:
// 
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double pauc(NumericVector idv, NumericVector dv, NumericVector range) {
  double tfirst = range[0];
  double tlast = range[1];
  dv = dv[idv >= tfirst];
  idv = idv[idv >= tfirst];
  dv = dv[idv <= tlast];
  idv = idv[idv <= tlast];
  double auc = 0;
  int n = idv.size() - 1;
  for(int i = 0; i < n; ++i) {
    auc += (dv[i] + dv[i+1])*(idv[i+1] - idv[i])/2;
  }
  return auc;
}
