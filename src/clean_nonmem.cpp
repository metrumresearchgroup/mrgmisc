#include <Rcpp.h>
#include <boost/algorithm/string.hpp>
#include <boost/regex.hpp>
#include <boost/algorithm/string/trim_all.hpp>
#include <string>
// [[Rcpp::depends(BH)]]
using namespace Rcpp;
// [[Rcpp::export]]
std::string clean_nonmem(std::vector<std::string> x, std::string sep, std::string colname) {
  int n = x.size();
  std::string str;
  std::string line_break = "\n";
  std::string table = "TABLE";
  for(int i = 0; i < n; ++i) {
  if(!boost::contains(x[i], table) && !boost::contains(x[i], colname)) {
    
      if(sep == "auto") {
      boost::trim(x[i]);
      boost::trim_all(x[i]);
        boost::replace_all(x[i], " ", ",");
      } else {
      x[i].erase(std::remove(x[i].begin(), x[i].end(),' '), x[i].end());
      }
    str.append(x[i]);
    str.append(line_break);
  }
  }
  return str;
}

// [[Rcpp::export]]
std::string clean_phi(std::vector<std::string> x, std::string sep, std::string colname) {
  int n = x.size();
  std::string str;
  std::string line_break = "\n";
  std::string table = "TABLE";
  for(int i = 0; i < n; ++i) {
  if(!boost::contains(x[i], table) && !boost::contains(x[i], colname)) {
    
      if(sep == "auto") {
      boost::trim(x[i]);
      boost::trim_all(x[i]);
        boost::replace_all(x[i], " ", ",");
      } else {
      x[i].erase(std::remove(x[i].begin(), x[i].end(),' '), x[i].end());
      }
    str.append(x[i]);
    str.append(line_break);
  }
  }
  return str;
}
