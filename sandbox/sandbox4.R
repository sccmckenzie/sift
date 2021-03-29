library(tidyverse)
library(lubridate)
library(sift)

a <- mopac %>%
  filter(toll_booth == "plaza",
         direction == "south",
         t > as_datetime("2020-05-19 10:00:00", tz = "US/Central"),
         t < as_datetime("2020-05-19 10:02:00", tz = "US/Central"))

b <- mopac %>%
  filter(toll_booth == "plaza",
         direction == "north",
         t > as_datetime("2020-05-19 11:50:00", tz = "US/Central"),
         t < as_datetime("2020-05-19 12:00:00", tz = "US/Central"))

bind_rows(a,b) %>%
  count(plate, sort = T) %>%
  filter(n == 2) %>%
  semi_join(bind_rows(a, b), .) %>%

