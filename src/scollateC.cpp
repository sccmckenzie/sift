#include <vector>
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<int>  scollateC(std::vector<double> x, std::vector<double> boundaries) {

  std::sort(boundaries.begin(), boundaries.end());

  std::vector<int> out(static_cast<int>(x.size()));

  for (int i{}; i < static_cast<int>(x.size()); ++i) {
    for (double element : boundaries) {
      if (x[i] >= element)
        out[i] += 1;
    }
  }

  return(out);
}
