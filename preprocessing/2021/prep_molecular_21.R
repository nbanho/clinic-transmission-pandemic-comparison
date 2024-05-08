#### Libraries ####

library(tidyverse)
library(readxl)


#### Data ####

df <- read_xlsx("data-raw/dfu-data-both-years.xlsx", sheet = "2023_first 3 batches stata") %>%
  rename(
    date = `collection date`,
    dna_copies = final_total_umbrella
  ) %>%
  filter(
    location == "Masi clinic",
    !is.na(date)
  ) %>%
  mutate(
    daytime = ifelse(hour(start_time) < 9, "Morning", "Afternoon"),
    room = ifelse(grepl("Waiting", room), "Waiting room", "TB room"),
    sampling_time = 3,
    date = as.Date(date)
  ) %>%
  dplyr::select(date, daytime, start_time, sampling_time, room, dna_copies)


#### Save data ####
saveRDS(df, "data-clean/2021/molecular.rds")
