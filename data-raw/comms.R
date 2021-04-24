library(lubridate)
library(dplyr)
library(tidyr)

# parameters
t0 <- as_datetime("1999-02-20 06:30:00", tz = "US/Central")
n <- 5e4

# dataset
set.seed(1)
comms <- tibble(station = rep(LETTERS[1:4], n/4),
                timestamp = t0 + rnorm(n, sd = 3600 * 24 * 7),
                msg_code = sample(1:(n/16), n, replace = TRUE),
                type = sample(c("receive", "send"), n, replace = TRUE)) %>%
  arrange(timestamp)

usethis::use_data(comms, overwrite = TRUE)
