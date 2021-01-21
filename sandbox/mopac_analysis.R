library(tidyverse)
library(lubridate)
library(sift)


# clock resolution for observation recording is too large (second)
# need to simulate
# at highway speed, 60 mph, two cars would be separated by at least 0.2 seconds

# adding `rbeta(n, 2, 5)` is a simple way to ensure there are no "exactly" overlapping vehicles
# plot(density(rbeta(200, 2, 5), bw = "SJ"))

# experiment with custom bw ("SJ" not appropriate here)
sift:::mopac_raw %>%
  mutate(time = time + dseconds(rbeta(n(), 2, 5))) %>%
  group_by(day) %>%
  mutate(tn = time_length(time - min(time), unit = "seconds"),
         k = kluster(time, bw = 2)) %>%
  ggplot(aes(tn)) +
  geom_density(bw = 2) +
  geom_rug(aes(color = factor(k)), size = 1) +
  facet_wrap(~ day, ncol = 1)


test_plot <- function(bw = "SJ") {
  params <- sift:::mopac_raw %>%
    mutate(time = time + dseconds(rbeta(n(), 2, 5))) %>%
    group_by(day) %>%
    mutate(k = kluster(time, bw = bw)) %>%
    group_by(k, .add = TRUE) %>%
    summarise(n = n(),
              mean_t = mean(time),
              sd_t = sd(time)) %>%
    mutate(d = time_length(mean_t - lag(mean_t), unit = "seconds")) %>%
    summarise(mean_n = mean(n),
              sd_n = sd(n),
              mean_d = mean(d, na.rm = TRUE),
              sd_d = sd(d, na.rNm = TRUE),
              mean_sd = mean(sd_t, na.rm = TRUE)) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))

  # craft virtual day

  k <- double()
  k[1] <- 0
  i <- 1

  while(TRUE) {
    i <- i + 1
    p <- k[i - 1] + rnorm(1, mean = params$mean_d, sd = params$sd_d)
    if(p > 150) {
      k <- k[-1]
      break
    } else {
      k[i] <- p
    }
  }

  test_data <- tibble(day = "test", k) %>%
    mutate(tn =  map(k, ~ {
      ..1 + rnorm(max(c(as.integer(rnorm(1, mean = params$mean_n, sd = params$sd_n)), 1L)), mean = 0, sd = params$mean_sd)
    }),
    k = row_number()) %>%
    unnest(cols = tn)

  message(nrow(test_data))

  sift:::mopac_raw %>%
    mutate(time = time + dseconds(rbeta(n(), 2, 5))) %>%
    group_by(day) %>%
    mutate(tn = time_length(time - min(time), unit = "seconds"),
           k = kluster(time, bw = 2)) %>%
    bind_rows(test_data) %>%
    ggplot(aes(tn)) +
    geom_density(bw = bw) +
    geom_rug(aes(color = factor(k)), size = 1) +
    facet_wrap(~ day, ncol = 1)
}

sift:::mopac_raw %>%
  drop_na() %>%
  count(day, make, model) %>%
  group_by(day) %>%
  mutate(wt = n / sum(n)) %>%
  group_by(make, model) %>%
  summarize(wt_sd = sd(wt),
            wt_mean = mean(wt),
            .groups = "drop") %>%
  arrange(desc(wt_mean))

sift:::mopac_raw %>% filter(day == "Wed") %>%
  mutate(t = time + dseconds(rbeta(n(), 2, 5))) %>%
  mutate(tn = time_length(t - min(t), unit = "seconds"),
         k = kluster(tn, bw = 2.0) %% 2) %>%
  ggplot(aes(tn)) +
  geom_density(bw = 2.0) +
  geom_rug(aes(color = factor(k)), size = 1)

t0 <- runif(1, 0, 1)

mopac %>%
  filter(checkpoint == "north_plaza",
         t > as_datetime("2020-05-19 00:00:00", tz = "US/Central") + dhours(t0 * 24),
         t < as_datetime("2020-05-19 00:00:00", tz = "US/Central") + dhours(t0 * 24) + dminutes(2.5)) %>%
  mutate(tn = time_length(t - min(t), unit = "seconds"),
         k = kluster(tn, bw = "SJ") %% 2) %>%
  ggplot(aes(tn)) +
  geom_density(bw = "SJ") +
  geom_rug(aes(color = factor(k)), size = 1)

mopac %>%
  ggplot(aes(t)) +
  geom_histogram(binwidth = 600) +
  facet_wrap(~ toll_booth, ncol = 1)

mopac %>%
  arrange(make, model, plate, t) %>%
  View()

mopac %>%
  group_by(make, model, plate) %>%
  summarize(n = n(), .groups = "drop") %>%
  count(n)
