#include <Rcpp.h>
#include <boost/algorithm/string.hpp>
#include <string>
// [[Rcpp::depends(BH)]]
using namespace Rcpp;
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
