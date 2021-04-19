#include <cpp11.hpp>
using namespace cpp11;

[[cpp11::register]]
doubles interlace(integers A_index, doubles A_x, integers B_index, doubles B_x) {
  int a = A_index.size() - 1;
  int b = A_x.size();

  writable::doubles out(b);

  for(int i = 0; i < a; ++i) {
    for(int j = A_index[i]; j < A_index[i + 1]; ++j) {
      out[j] = NA_REAL;
      if (is_na(A_x[j])) {
        continue;
      } else {
        for(int k = B_index[i]; k < B_index[i + 1]; ++k) {
          if (is_na(B_x[k])) {
            out[j] = NA_REAL;
            continue;
          } else if (B_x[k] > A_x[j]) {
            out[j] = B_x[k]; // can we just return k instead?
            break;
          } else {
            continue;
          }
        }
      }
    }
  }

  return(out);
}
