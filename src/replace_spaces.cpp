#include <Rcpp.h>
#include <boost/algorithm/string/replace.hpp>
#include <string>
// [[Rcpp::depends(BH)]]
using namespace Rcpp;

// [[Rcpp::export]]
std::string replace_spaces(std::vector<std::string> x) {
  int n = x.size();
  std::string str;
  std::string line_break = "\n";
  for(int i = 0; i < n; ++i) {
  boost::replace_all(x[i], "  ", ",");
  str.append(x[i]);
  str.append(line_break);
  }
  return str;
}
