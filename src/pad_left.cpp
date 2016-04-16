#include <Rcpp.h>
using namespace Rcpp;


std::string pad(std::string string, int n, std::string paddingChar) {
  while(string.size() < n) {
    string = paddingChar + string;
  }
  return string;
}

// [[Rcpp::export]]
std::vector< std::string > padLeft( std::vector< std::string > strings, int numChars, std::string paddingChar) {
  
  int len = strings.size();
  
  for( int i=0; i < len; i++ ) {
    strings[i] = pad(strings[i], numChars, paddingChar);
  }
  
  return strings;
}
/*** R
padLeft(c("1", "10", "100"), 5, "0")
*/
