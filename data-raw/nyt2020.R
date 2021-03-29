library(tidyverse)
library(lubridate)
library(glue)
library(jsonlite)

nyt_raw <- map_dfr(1:12, ~{
  df <- fromJSON(glue("https://api.nytimes.com/svc/archive/v1/2020/{..1}.json?api-key=EnmkuDwRIxLKG93QJb1Pp3lwAIxVTKod"))

  df$response$docs %>%
    as_tibble()
})


major_events <- c("Trump Tests Positive for the Coronavirus",
                  "Joe Biden is elected the 46th president of the United States.",
                  "Monolith Discovered in Utah Desert")

set.seed(15)

nyt2020 <- nyt_raw %>%
  filter(section_name %in% c("U.S.", "World", "Business Day", "New York", "Technology", "Education", "Health", "Science")) %>%
  transmute(headline = headline$main,
            abstract,
            byline = byline$original,
            pub_date = as_datetime(pub_date) %>% date(),
            section_name,
            web_url) %>%
  distinct() %>%
  mutate(wt = if_else(headline %in% major_events, 100, 1)) %>%
  with_groups(pub_date, sample_n, 5, weight = wt) %>%
  select(-wt)

usethis::use_data(nyt2020, overwrite = TRUE)
