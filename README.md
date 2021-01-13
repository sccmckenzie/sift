
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sift

<!-- badges: start -->
<!-- badges: end -->

sift facilitates **intelligent** & **efficient** exploration of
datasets.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("sccmckenzie/sift")
```

3 functions are provided:

-   `sift` - *augmented* dataset subsetting. Imagine `dplyr::filter()`
    that includes neighboring observations.

-   `klassify` - reveal implicit grouping in continuous data. Imagine 1D
    KNN - except you don’t have to provide K.

-   `kollate`

## klassify

``` r
library(sift)

x <- c(1, 2, 2, 5, 3000)
```

By value, a natural split for `x` would be:

-   **Group 1**: 1, 2, 2, 5
-   **Group 2**: 3000

`klassify` does this automatically

``` r
klassify(x)
#> [1] 1 1 1 1 2
```
