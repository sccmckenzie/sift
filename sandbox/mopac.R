library(tidyverse)
library(lubridate)
library(readxl)

time_datum <- c("2020-05-17 17:27:00",
                "2020-05-18 18:24:00",
                "2020-05-19 18:31:00",
                "2020-05-20 18:27:00",
                "2020-05-21 18:47:00",
                "2020-05-22 18:44:00",
                "2020-05-23 15:04:00") %>%
  as_datetime(tz = "US/Central")

mopac_raw <- map_dfr(.x = 1:7, .f = ~ {
  read_excel("data-raw/mopac.xlsx", sheet = ..1) %>%
    mutate(day = wday(..1, label = TRUE),
           time = time_datum[[..1]] + dseconds(time))
}) %>%
  janitor::clean_names() %>%
  relocate(c(day, time), .before = everything())

mopac_raw %>%
  drop_na() %>%
  count(day, make, model) %>%
  group_by(day) %>%
  mutate(wt = n / sum(n)) %>%
  group_by(make, model) %>%
  summarize(wt = mean(wt), .groups = "drop") %>%
  arrange(desc(wt))

mopac_raw
