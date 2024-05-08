#### Libraries ####

library(tidyverse)
library(haven)
library(lubridate)

source("utils/trans_risk.r")

#### Data ####

df <- read_dta("data-raw/2019/environmental/CleanCo2data.dta") %>%
  rename(
    room = location,
    date = RecDate,
    time = RecTime,
    temperature = temp,
    humidity = relhum,
    co2 = co2
  ) %>%
  filter(room %in% c("WaitingRoom", "TBroom", "EndOfPassage")) %>%
  mutate(
    room = case_when(
      room == "WaitingRoom" ~ "Waiting room",
      room == "TBroom" ~ "TB room",
      room == "EndOfPassage" ~ "Corridor"
    ),
    date_time = round_date(as.POSIXct(paste(date, format(time, format = "%H:%M:%S"))))
  ) %>%
  dplyr::select(date, room, date_time, co2, temperature, humidity)


#### Filter ####

# plot CO2 data
df %>%
  ggplot(aes(x = date_time, y = co2, color = room)) +
  geom_line() +
  facet_wrap(~date, scales = "free_x")

# subset CO2 data
df <- df %>%
  filter(date %in% paste0("2019-", c(
    "07-25", "07-26", "07-29", "07-30", "07-31",
    "08-01", "08-02", "08-05", "08-06", "08-07",
    "08-08", "08-12", "08-16", "08-19", "08-20",
    "08-21", "08-22", "08-23"
  )))

#### Save data ###
saveRDS(df, "data-clean/2019/environmental.rds")
