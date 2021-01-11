#include <vector>
#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;

double c1;

double sdnorm(double y) {
  return c1 * std::exp(-0.5 * y * y);
}

// [[Rcpp::export]]
std::vector<double> scdensity(std::vector<double>& x, std::vector<double>& kords, const double h, const double pi) {
  c1 = sqrt(1 / (2 * pi));

  std::vector<double> out(kords.size());

  for (int i{ 0 }; i < static_cast<int>(kords.size()); ++i) {
    for (double element : x) {
      out[i] += sdnorm((element - kords[i]) / h);
    }

    out[i] /= static_cast<double>(x.size()) * h;
  }

  return out;
}
