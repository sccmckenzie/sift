library(tidyverse)
library(lubridate)
library(sift)

set.seed(1)
df <- mopac %>%
  sample_n(10) %>%
  arrange(t)

df %>%
  sift(t, dhours(2), model == "Ridgeline")

df %>%
  sift(t, dhours(2), model %in% c("Ridgeline", "911"))





df <- mopac %>%
  count(plate, sort = TRUE) %>%
  slice_head(n = 10) %>%
  semi_join(mopac, .)

df %>%
  arrange(t) %>%
  # mutate(t1 = t - dminutes(45),
  #        t2 = t + dminutes(45)) %>%
  with_groups(plate, mutate, I = t == min(t)) %>%
  group_by(plate) %>%
  sift(t, dminutes(10), I) %>%
  arrange(plate, t) %>%
  print(n = nrow(.))



sift_compare <- function() {
  df %>%
    mu
}

mopac %>%
  group_by(row_number()) %>%
  sift(t, dminutes(3), TRUE)


temp <- mopac %>%
  # mutate(I = row_number() %in% c(3, 7)) %>%
  mutate(I = TRUE) %>%
  group_by(row_number()) %>%
  mutate(t1 = t - dminutes(3),
         t2 = t + dminutes(3),
         t1 = if_else(I, t1, NA_POSIXct_),
         t2 = if_else(I, t2, NA_POSIXct_))


# below is the bottleneck that could be optimized in C++

temp %>%
  fill(t1, .direction = "up") %>%
  fill(t2, .direction = "down")# %>%
# filter(t > t1 | t < t2)


library(bench)
library(testthat)

set.seed(17)
x <- runif(1e6, min = 0, max = 1e7)
y <- sort(runif(1e5, min = 0, max = 1e7))


findInterval(x, y)
sift:::scollate(x, y)

expect_equal()
