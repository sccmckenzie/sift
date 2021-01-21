library(tidyverse)
library(lubridate)
library(readxl)
library(sift)

time_datum <- c("2020-05-17 17:27:00",
                "2020-05-18 18:24:00",
                "2020-05-19 18:00:00",
                "2020-05-20 18:00:00",
                "2020-05-21 18:00:00",
                "2020-05-22 18:00:00",
                "2020-05-23 15:00:00") %>%
  as_datetime(tz = "US/Central")

mopac_raw1 <- map_dfr(.x = 1:2, .f = ~ {
  read_excel("data-raw/mopac.xlsx", sheet = ..1) %>%
    mutate(day = wday(..1, label = TRUE),
           time = time_datum[[..1]] + dseconds(time))
})

mopac_raw2 <- map_dfr(.x = 3:7, .f = ~ {
  read_excel("data-raw/mopac.xlsx", sheet = ..1) %>%
    mutate(day = wday(..1, label = TRUE),
           time = sprintf("%.2f", time),
           time = time_datum[[..1]] + dminutes(as.integer(str_extract(time, "^\\d+"))) + dseconds(as.integer(str_extract(time, "\\d+$"))))
})

mopac_raw <- bind_rows(mopac_raw1, mopac_raw2) %>%
  janitor::clean_names() %>%
  relocate(c(day, time), .before = everything())

usethis::use_data(mopac_raw, overwrite = TRUE, internal = TRUE)
