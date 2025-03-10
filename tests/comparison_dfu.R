#### Library ####

library(tidyverse)
library(lubridate)
library(readxl)

#### Data ####

dfu19 <- read_xlsx("data-raw/2019/molecular/2020_04 DFU results_Zeurcher.xlsx", sheet = "stata") %>%
  filter(Location == "Masi clinic") %>%
  rename(room = `Venue name`,
         dna_copies = `sum of averageA&B_excl droplets`,
         date = Date) %>%
  filter(room %in% c("Waiting area", "TB Room")) %>%
  mutate(room = ifelse(room == "Waiting area", "Waiting room", "TB room"),
         date = as.Date(date),
         time = ifelse(hour(Start_time) < 12, "10:30", "14:00"),
         year = "2019 (original)") %>%
  dplyr::select(year, date, time, room, dna_copies)

dfu19reworked <- read_xlsx("data-raw/dfu-data-both-years.xlsx", sheet = "2019 data reworked stata") %>%
  rename(date = `Collection Date`,
         dna_copies = final_total_umbrella,
         room = `Venue name`) %>%
  filter(Location == "Masi clinic",
         !is.na(date)) %>%
  mutate(time = ifelse(hour(Start_time) < 12, "10:30", "14:00"),
         room = ifelse(room == "Waiting area", "Waiting room", "TB room"),
         year = "2019 (reworked)") %>%
  dplyr::select(year, date, time, room, dna_copies)

dfu21 <- read_xlsx("data-raw/dfu-data-both-years.xlsx", sheet = "2023_first 3 batches stata") %>%
  rename(date = `collection date`,
         dna_copies = final_total_umbrella) %>%
  filter(location == "Masi clinic",
         !is.na(date)) %>%
  mutate(time = ifelse(hour(start_time) < 9, "08:00:00", "11:00:00"),
         room = ifelse(grepl("Waiting", room), "Waiting room", "TB room"),
         year = "2021") %>%
  dplyr::select(year, date, time, room, dna_copies) 

rbind(dfu19, dfu19reworked, dfu21) %>% 
  filter(room == "Waiting room") %>%
  rename(`Sampling time` = time) %>%
  ggplot(aes(x = year, y = log1p(dna_copies), fill = `Sampling time`)) + #
  geom_boxplot(position = position_dodge(width = 1)) +
  labs(y = "Log DNA copies", title = "Waiting room") +
  theme(axis.title.x = element_blank())
