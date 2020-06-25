#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
std::vector<std::string> replace_chars(std::vector<std::string> strings, 
                                       std::vector<std::string> values, 
                                       std::vector<std::string> replacement) {
  int num_strings = strings.size();
  int num_values = values.size();
  for (int i = 0; i < num_strings; i++) { 
    for (int j =0; j < num_values; j++) {
      if(strings[i] == values[j]) {
        strings[i] = replacement[j];
        break;
      } else {
        continue;
      }
    }
  }
  return strings;
}
