library(ggplot2)
library(dplyr)
library(lubridate)

set.seed(20)
us_uk_pop <- tidyr::population %>%
  filter(country %in% c("United States of America", "United Kingdom of Great Britain and Northern Ireland")) %>%
  transmute(country = if_else(stringr::str_detect(country, "United States"), "USA", "UK"),
            date = make_date(year) + rnorm(n(), mean = 19, sd = 1),
            population) %>%
  arrange(desc(country))

us_uk_leaders <- presidential %>%
  filter(year(start) > 1988) %>%
  transmute(country = "USA",
            name,
            start,
            party) %>%
  add_row(country = "UK",
          name = c("Thatcher", "Major", "Blair", "Brown", "Cameron", "May", "Johnson"),
          start = as_date(c("1979-05-04", "1990-11-28", "1997-05-02", "2007-06-27", "2010-05-11", "2016-07-13", "2019-07-24")),
          party = c("Conservative", "Conservative", "Labour", "Labour", "Conservative", "Conservative", "Conservative"))

usethis::use_data(us_uk_pop, overwrite = TRUE)
usethis::use_data(us_uk_leaders, overwrite = TRUE)
