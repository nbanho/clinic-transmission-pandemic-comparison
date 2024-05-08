#### Libraries ####

library(tidyverse)
library(readxl)


#### Data ####

df <- read_xlsx("data-raw/dfu-data-both-years.xlsx", sheet = "2019 data reworked stata") %>%
  rename(
    date = `Collection Date`,
    dna_copies = final_total_umbrella,
    room = `Venue name`,
    start_time = Start_time
  ) %>%
  filter(
    Location == "Masi clinic",
    !is.na(date)
  ) %>%
  mutate(
    start_time = as.POSIXct(paste(date, format(start_time, "%H:%M:%S"))),
    daytime = ifelse(hour(start_time) < 12, "Morning", "Afternoon"),
    room = ifelse(room == "Waiting area", "Waiting room", "TB room"),
    sampling_time = 2,
  ) %>%
  dplyr::select(date, daytime, start_time, room, sampling_time, dna_copies)


#### Save data ####
saveRDS(df, "data-clean/2019/molecular.rds")
