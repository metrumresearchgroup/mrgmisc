#include <Rcpp.h>
using namespace Rcpp;
//' calculate partial AUC
//' @param time vector of time values
//' @param dv concentration measurements
//' @param range vector of min and max value of the partial auc range
//' @examples \dontrun{
//' library(PKPDdatasets)
//' library(dplyr)
//' sd_oral_richpk %>% group_by(ID) %>% 
//'   summarize(pauc0_12 = auc_partial_cpp(Time, Conc, c(0, 12)))
//' }
// [[Rcpp::export]]
double auc_partial_cpp(NumericVector time, NumericVector dv, NumericVector range) {
  if (time.size() != dv.size()) {
    stop("length of time and dv vectors do not match");
  }
  double min_time = range[0];
  double max_time = range[1];
  int n = time.size();
  double pauc = 0;
  // will become the min value
  // one less than final index as need to reference the i+1 index
  // in the summation calculation
  for(int i = 0; i < n-1; ++i) {
    if(time[i+1] > max_time) {
      return(pauc);
    }
    if (time[i] < min_time) {
      continue;
    }
    // should not have NA values here at this point
    // this function should just sum for the moment, need to 
    // determine if want to do interpolation strategy in this function or elsewhere
    // preference is to do it elsewhere
    if (R_IsNA(dv[i])) {
      return(NA_REAL);
    } 
    pauc += (dv[i] + dv[i+1])*(time[i+1] - time[i])/2;
      
  }
  return pauc;
}
