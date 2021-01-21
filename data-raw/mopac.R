library(tidyverse)
library(lubridate)
library(meandr)
library(sift)

# define parameters
profile_dual <- create_path(n_points = 512,
                              nodes = c(0.1, 1, -1.4, 0, 0.6, 0, -1, 0))

profile_late <- create_path(n_points = 512,
                             nodes = c(0.1, 1, 0, 2, -5, -3))

profile_early <- create_path(n_points = 512,
                             nodes = c(0.1, 1, -1.4, 0, 0, 0, 0.3, 0, 0))

start_time <- as_datetime("2020-05-19 00:00:00", tz ="US/Central")
bw <- 2.0
vehicle_density <- function(t, profile, alpha = 9) {
  profile <- profile %>%
    mutate(f = f * alpha)

  tn <- time_length(t - floor_date(t, unit = "days"), unit = "hours") / 24

  approx(profile, xout = tn, yleft = 0, yright = 0)$y
}

# extract stats from real dataset
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
            sd_d = sd(d, na.rm = TRUE),
            mean_sd = mean(sd_t, na.rm = TRUE)) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))


# define simulation function
simulate_traffic <- function(profile, alpha, beta, gamma) {
  tk <- start_time
  i <- 1
  while(TRUE) {
    i <- i + 1
    tp <- tk[i - 1] + rnorm(1, mean = params$mean_d * alpha, sd = params$sd_d * alpha)

    if (tp > start_time + ddays(1)) {
      tk <- tk[-1]
      break
    }
    tk[i] <- tp
  }

  tibble(tk) %>%
    mutate(t =  map(tk, ~ {
      ..1 + rnorm(
        max(
          c(
            as.integer(
              rnorm(1,
                    mean = vehicle_density(..1, profile),
                    sd = params$sd_n * beta)
            ),
            1L
          )
        ),
        mean = 0,
        sd = params$mean_sd * gamma
      )
    }),
    k = row_number()) %>%
    unnest(cols = t) %>%
    select(-tk, -k)
}


# generate data

# alpha -> k spacing coefficient (affects mean + sd)
# beta -> variation in no. of vehicles / cluster from given vehicle_density at that time
# gamma -> spacing of vehicles within cluster

set.seed(2048)
mopac <- tribble(~ toll_booth, ~ profile, ~ alpha, ~beta, ~gamma,
                 "south_45", profile_late, 3, 0.5, 3,
                 "north_45", profile_early, 3, 0.5, 3,
                 "south_plaza", profile_dual, 1, 1, 1,
                 "north_plaza", profile_dual, 1, 1, 1) %>%
  mutate(data = pmap(.l = list(profile, alpha, beta, gamma), simulate_traffic)) %>%
  select(-profile, -alpha, -beta, -gamma) %>%
  unnest(cols = data) %>%
  arrange(toll_booth, t)

# generate vehicles
# use Texas license plate format: AAA-0000

n_vehicles <- nrow(mopac) * 0.75 # most vehicles will have 2 timestamps. Not all vehicles generated will be used in dataset

vehicle_probs <- sift:::mopac_raw %>%
  drop_na() %>%
  count(day, make, model) %>%
  group_by(day) %>%
  mutate(wt = n / sum(n)) %>%
  group_by(make, model) %>%
  summarize(wt_mean = mean(wt), .groups = "drop") %>%
  mutate(wt = wt_mean / sum(wt_mean), .keep = "unused")

color_probs <-  sift:::mopac_raw %>%
  drop_na() %>%
  count(make, model, color) %>%
  group_by(make, model) %>%
  mutate(n = n / sum(n)) %>%
  ungroup()

set.seed(2049)
plate_letters <- crossing(L1 = LETTERS, L2 = LETTERS, L3 = LETTERS) %>%
  mutate(st = str_c(L1, L2, L3, sep = "")) %>%
  pull(st) %>%
  sample(., n_vehicles, replace = TRUE)

plate_numbers <- 0:9999
plate_numbers <- str_pad(plate_numbers, side = "left", pad = "0", width = 4) %>%
  sample(., n_vehicles, replace = TRUE)

plates <- str_c(plate_letters, plate_numbers, sep = "-")


vehicles <- tibble(plate = unique(plates)) %>%
  bind_cols(sample_n(vehicle_probs, size = nrow(.), replace = TRUE, weight = wt)) %>%
  left_join(color_probs) %>%
  group_by(plate) %>%
  sample_n(1, weight = n) %>%
  ungroup() %>%
  select(-c(wt, n)) %>%
  sample_n(nrow(.))

travel_plans <- tribble(~path, ~prob, ~size,
                        "plaza_commute", 07, 2,
                        "45_commute", 0.2, 2,
                        "plaza_45_thru", 0.05, 2,
                        "45_plaza_thru", 0.05, 2)

extract_timestamps <- function(booth1, booth2) {
  if(nrow(filter(mopac_shuffle, toll_booth == booth1)) == 0) {
    break
  }

  enter <- mopac_shuffle %>%
    filter(toll_booth == booth1) %>%
    sample_n(1)

  if(nrow(filter(mopac_shuffle, toll_booth == booth2, t > enter$t)) == 0) {
    output <<- enter %>%
      mutate(vehicle = vehicle_i) %>%
      bind_rows(output)

    mopac_shuffle <<- mopac_shuffle[mopac_shuffle$i != enter$i,]
  } else {
    exit <- mopac_shuffle %>%
      filter(toll_booth == booth2,
             t > enter$t) %>%
      sample_n(1)

    output <<- bind_rows(enter, exit) %>%
      mutate(vehicle = vehicle_i) %>%
      bind_rows(output)

    mopac_shuffle <<- mopac_shuffle[!mopac_shuffle$i %in% c(enter$i, exit$i),]
  }

  vehicle_i <<- vehicle_i + 1
}

set.seed(2050)
mopac_shuffle <- mopac %>%
  slice_sample(n = nrow(.)) %>%
  mutate(i = row_number())

vehicle_i <- 1
output <- tibble(toll_booth = character(), t = POSIXct(tz = "US/Central"), i = integer(), vehicle = double())

# simulate vehicles commuting and passing through town (warning: this takes at least 5min to run!)
while(nrow(output) < 80e3) {
  path <- sample_n(travel_plans, size = 1, weight = prob)

  switch(path$path,
    "plaza_commute" = extract_timestamps("south_plaza", "north_plaza"),
    "45_commute" = extract_timestamps("north_45", "south_45"),
    "plaza_45_thru" = extract_timestamps("south_plaza", "south_45"),
    "45_plaza_thru" = extract_timestamps("north_45", "north_plaza")
  )

  if((nrow(output) + nrow(mopac_shuffle)) != nrow(mopac)) {
    message("row count mismatch")
    break
  }
}

irregular_timestamps <- function(booth1, booth2) {
  enter <- mopac_shuffle %>%
    filter(toll_booth == booth1) %>%
    sample_n(1)

  if(nrow(filter(mopac_shuffle, toll_booth == booth2, t > enter$t)) < 2) {
    output <<- enter %>%
      mutate(vehicle = vehicle_i) %>%
      bind_rows(output)

    mopac_shuffle <<- mopac_shuffle[mopac_shuffle$i != enter$i,]
  } else {

    exit <- mopac_shuffle %>%
      filter(toll_booth == booth2,
             t > enter$t) %>%
      sample_n(2)

    output <<- bind_rows(enter, exit) %>%
      arrange(t) %>%
      mutate(vehicle = vehicle_i) %>%
      bind_rows(output)

    mopac_shuffle <<- mopac_shuffle[!mopac_shuffle$i %in% c(enter$i, exit$i),]
  }

  vehicle_i <<- vehicle_i + 1
}

set.seed(2050)
while(nrow(output) < 82e3) {
  path <- sample_n(travel_plans, size = 1, weight = prob)

  switch(path$path,
         "plaza_commute" = irregular_timestamps("south_plaza", "north_plaza"),
         "45_commute" = irregular_timestamps("north_45", "south_45"),
         "plaza_45_thru" = irregular_timestamps("south_plaza", "south_45"),
         "45_plaza_thru" = irregular_timestamps("north_45", "north_plaza")
  )

  if((nrow(output) + nrow(mopac_shuffle)) != nrow(mopac)) {
    message("row count mismatch")
    break
  }
}

# bank robbery
robber_enter <- mopac_shuffle %>%
  filter(toll_booth == "south_plaza",
         t > as_datetime("2020-05-19 10:00:00", tz = "US/Central"),
         t < as_datetime("2020-05-19 10:02:00", tz = "US/Central")) %>%
  sample_n(2)

robber_exit <- mopac_shuffle %>%
  filter(toll_booth == "north_plaza",
         t > as_datetime("2020-05-19 11:50:00", tz = "US/Central"),
         t < as_datetime("2020-05-19 12:00:00", tz = "US/Central")) %>%
  sample_n(2)

mopac_shuffle <- mopac_shuffle[!mopac_shuffle$i %in% c(robber_enter$i, robber_exit$i),]

output <- bind_rows(
  mutate(robber_enter, vehicle = vehicle_i:(vehicle_i+1)),
  mutate(robber_exit, vehicle = vehicle_i:(vehicle_i+1)),
  output
)

vehicle_i <- vehicle_i + 2

# assign remainder of observations to unique vehicle
mopac <- mopac_shuffle %>%
  mutate(vehicle = seq(vehicle_i, vehicle_i + nrow(mopac_shuffle) - 1)) %>%
  bind_rows(output) %>%
  left_join(vehicles %>% mutate(vehicle = row_number())) %>%
  select(-c(i, vehicle)) %>%
  separate(toll_booth, into = c("direction", "toll_booth")) %>%
  relocate(t, toll_booth, direction, .before = everything()) %>%
  arrange(t)

usethis::use_data(mopac, overwrite = TRUE)

