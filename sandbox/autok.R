library(dplyr)

x_input <- c(-95, -70, -65, -50, 50, 65, 70, -71)

hmean <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]

  length(x) / sum(1 / x)

}

autok <- function(x) {
  # sort input vector (this may be used later)
  xs <- sort(x, na.last = TRUE, index.return = TRUE)

  # remove missing values
  xo <- xs$x[!is.na(xs$x)]

  # normalize so that all data is between 0 and 1
  xn <- (xo - min(xo)) / (max(xo - min(xo)))

  # calculate distances between successive points
  dxn <- xn[-1] - xn[-length(xn)]

  # calculate k
  k <- c(1, (cumsum(dxn/hmean(dxn) > 2) + 1))

  # sort
  k[sort(xs$ix, index.return = TRUE)$ix]
}


plot(autok(x_input), x_input)

test <- c(NA, 100, 100, runif(9, 0, 1000)) %>% round(0)

sort(test, index.return = TRUE, na.last = TRUE) %>%
  as_tibble()
