library(tidyverse)
library(lubridate)
library(sift)
library(mopac)

df <- express %>%
  arrange(time) %>%
  group_by(plate) %>%
  sift(time, dseconds(5), TRUE) %>%
  summarize(n = n(),
            nc = n_distinct(.cluster))




nyt2020 %>%
  sift(pub_date, scope = 2, str_detect(headline, "Monolith")) %>%
  select(.anchor, headline, abstract)

# a <- runif(10, min = 0, max = 100)
# a <- round(a, 0)
# a[runif(1, 0, 10)] <- NA_real_
#
#
#
# tibble(a, k = kluster(a, bw = 20, fixed = TRUE)) %>%
#   ggplot(aes(a, 1, color = factor(k))) +
#   geom_point()
#
# tibble(a) %>%
#   ggplot(aes(a, 1)) +
#   geom_point()
