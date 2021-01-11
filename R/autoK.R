#' Automatically cluster 1-dimensional continuous data
#'
#' @param x Vector to be clustered. Must contain at least 1 non-missing value.
#'
#' @return
#' An integer vector identifying the cluster corresponding to each element in \code{x}.
#' @export
#'
#' @examples
#' 1
autoK <- function(x) {

  # Coerce x to numeric
  tryCatch(
    warning = function(cnd) {
      stop("input x must be coercible to <numeric>")
    },
    x <- as.numeric(x)
  )

  # Handle missing data while preserving indices
  xdf <- data.frame(i = seq_along(x),
                    x = x,
                    out = NA_integer_)

  xnm <- xdf[!is.na(xdf$x), ]
  xnm <- xnm[order(xnm$x), ]
  nnm <- nrow(xnm)


  if (nnm < 1) {
    stop("input x must contain at least 1 non-missing value")
  }

  if (nnm == 1) {
    xdf$out[xnm$i] <- 1L
    return(xdf$out)
  }

  # kernel density estimation
  d <- stats::density(xnm$x, bw = "SJ")
  tp <- pastecs::turnpoints(d$y)
  boundaries <- d$x[tp$pits]

  xnm$out <- scollateC(xnm$x, boundaries) + 1

  result <- rbind(xdf[is.na(xdf$x), ], xnm)

  return(result[order(result$i), ]$out)
}
