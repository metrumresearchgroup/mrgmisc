#include <Rcpp.h>
#include <boost/algorithm/string.hpp>
#include <string>
// [[Rcpp::depends(BH)]]
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
std::string clean_nonmem(std::vector<std::string> x) {
  int n = x.size();
  std::string str;
  std::string line_break = "\n";
  std::string table = "TABLE";
  std::string colname = "ID";
  for(int i = 0; i < n; ++i) {
  if(!boost::contains(x[i], table) && !boost::contains(x[i], colname)) {
    boost::trim(x[i]);
    boost::replace_all(x[i], "  ", ",");
    str.append(x[i]);
    str.append(line_break);
  }
  }
  return str;
}
