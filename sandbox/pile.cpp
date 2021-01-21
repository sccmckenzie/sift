#include <vector>
#include <algorithm>
#include <cmath>
#include <Rcpp.h>

using namespace Rcpp;

std::vector<double> currentAnchors{};

void populateAnchors(const int an, const int i, std::vector<int>& anchor_grps, std::vector<int>& grps, std::vector<double>& anchors) {
  currentAnchors.clear();

  for (int j{ 0 }; j < an; ++j) {
    if (anchor_grps[j] == grps[i])
      currentAnchors.push_back(anchors[j]);
    else
      continue;
  }

  std::sort(currentAnchors.begin(), currentAnchors.end());
}

// [[Rcpp::export]]
List pile(std::vector<double> x, std::vector<int> grps, std::vector<double> anchors, std::vector<int> anchor_grps) {

  const int xn{ static_cast<int>(x.size()) };
  const int an{ static_cast<int>(anchors.size()) };

  std::vector<double> outL(static_cast<int>(x.size()));
  std::vector<double> outU(static_cast<int>(x.size()));

  for (int i{ 0 }; i < xn; ++i) {

    // define vector of anchors sharing same group as x[i]
    if (i == 0) {
      populateAnchors(an, i, anchor_grps, grps, anchors);
    }
    else if (grps[i] != grps[i - 1]) {
      populateAnchors(an, i, anchor_grps, grps, anchors);
    }

    // walk along anchors sharing same group as x[i]
    for (auto a{ currentAnchors.begin() }; a != currentAnchors.end(); ++a) {
      if (x[i] <= *a) {
        outL[i] = *a - x[i];
        break;
      }
      else {
        outL[i] = NAN;
        continue;
      }
    }
    // reverse walk along anchors sharing same group as x[i]
    for (auto a{ currentAnchors.rbegin() }; a != currentAnchors.rend(); ++a) {
      if (x[i] >= *a) {
        outU[i] = x[i] - *a;
        break;
      }
      else {
        outU[i] = NAN;
        continue;
      }
    }
  }

 return List::create(outL, outU);
}
