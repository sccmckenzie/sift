density(c(-20, rep(0,98), 20))
plot(density(c(-20, rep(0,98), 20)), xlim = c(-4, 4))

d <- density(faithful$eruptions, bw = 0.14)

plot(d)

plot(d, type = "n")
polygon(d, col = "wheat")

x <- xx <- faithful$eruptions
x[i.out <- sample(length(x), 10)] <- NA
doR <- density(x, bw = "SJ", na.rm = TRUE)
lines(doR, col = "blue")
points(xx[i.out], rep(0.01, 10))


str(d)

plot(d$x, d$y)



tp <- turnpoints(d$y)

plot(d)

points(d$x[tp$pits],d$y[tp$pits],col = "red")




plot(faithful$eruptions)

min(faithful$eruptions)

max(faithful$eruptions)


x <- seq(1, 5.5, length.out = 1000)
h <- bw.SJ(faithful$eruptions)

map_dbl(x, ~ {
  sum(dnorm((faithful$eruptions - ..1)/h)) / (length(faithful$eruptions) * h)
}) %>%
  tibble(x,
         fh = .) %>%
  ggplot() +
  geom_line(aes(x, fh)) +
  geom_line(aes(x, y), color = "blue", data = tibble(x = d$x, y = d$y)) +
  geom_rug(aes(x = eruptions), data = faithful)


plot(density(c(rnorm(100, mean = 0), rnorm(100, mean = 100)), bw = "SJ"))
plot(density(c(1, 2, 4, 7, 10, 20, 100), bw = "SJ"))

# stress test bw.SJ
map_dbl(1:100, ~ {
  bw.SJ(runif(1e6))
})




# library(cpp11)
#
#
# cpp_source(code = '
# #include "cpp11/doubles.hpp"
# using namespace cpp11;
# namespace writable = cpp11::writable;
#
# double c1;
#
# double sdnorm(double y) {
# 	return c1 * std::exp(-0.5 * y * y);
# }
#
# [[cpp11::register]]
# doubles scdensity(doubles x, doubles kords, double h, double pi) {
# 	c1 = sqrt(1 / (2 * pi));
#
# 	doubles out(kords.size());
#
# 	for (int i{ 0 }; i < static_cast<int>(kords.size()); ++i) {
# 		for (double element : x) {
# 			out[i] += sdnorm((element - kords[i]) / h);
# 		}
#
# 		out[i] /= static_cast<double>(x.size()) * h;
# 	}
#
# 	return out;
# }')




auto_sift <- function(x) {
  # check numeric
  # more than 1 non-missing data point
  # remove missing values while preserving indices

  # bandwidth
  h <- bw.SJ(x)
  # h <- 0.14

  # density range
  from <- min(x) - 3 * h
  to <- max(x) + 3 * h

  n <- max(length(x), 512)
  if (n > 512) n <- 2^ceiling(log2(n)) #- to be fast with FFT

  kords <- seq(from, to, length.out = n)
  # kords <- c(0, 0.5, 1)


  # map_dbl(kords, ~ {
  #   sum(dnorm((x - ..1)/h)) / (length(x) * h)
  # })

  scdensity(x, kords, h, pi)

  return(TRUE)
}

compare_density <- function(x) {
  density(x, bw = "SJ")$y

  return(TRUE);
}

plot(auto_sift(c(0.25, 0.25, 0.75)), compare_density(c(0.25, 0.25, 0.75)))


# conclusion is that using built-in density is optimal
bench::mark(
  auto_sift(rnorm(1000)),
  compare_density(rnorm(1000))
)



