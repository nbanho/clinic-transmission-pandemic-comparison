#### Libraries ####

library(tidyverse)
library(lubridate)
source("utils/trans_risk.r")


#### Data ####

env <- readRDS("data-clean/2019/environmental.rds")
track_count <- readRDS("data-clean/2019/tracking-no_of_people.rds")
pers_time <- readRDS("data-clean/2019/tracking-person_time.rds")
mol <- readRDS("data-clean/2019/molecular.rds")
clin <- readRDS("data-clean/2019/clinical.rds")

#### Air change rate ####

# combine co2 and tracks
vent <- env %>%
  filter(room == "Waiting room") %>%
  filter(between(hour(date_time), 8, 15)) %>%
  mutate(date_time = floor_date(date_time, "minute")) %>%
  mutate(daytime = ifelse(hour(date_time) < 11, "Morning", "Afternoon")) %>%
  dplyr::select(date, date_time, daytime, room, co2) %>%
  left_join(track_count) %>%
  nest(data = c(date_time, co2, n))

# compute AER
vent$data <- lapply(vent$data, function(x) mutate(x, vol = 10.55 * 5.7 * 3))
vent$data <- lapply(vent$data, function(x) {
  x %>%
    rename(C = co2, V = vol) %>%
    mutate(C1 = dplyr::lead(C)) %>%
    na.omit()
})

# compute air change rates per day and room
vent$aer <- sapply(vent$data, estimate_aer)

vent %>%
  ggplot(aes(x = date, y = aer, fill = daytime)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_date(date_breaks = "day", date_labels = "%b %d")

# subset
vent <- dplyr::select(vent, date, daytime, room, aer)


#### Humidity and temperature ####

# take the mean
temp_humid <- env %>%
  filter(between(hour(date_time), 8, 14)) %>%
  mutate(daytime = ifelse(hour(date_time) < 11, "Morning", "Afternoon")) %>%
  group_by(date, room, daytime) %>%
  summarise(
    temperature = mean(temperature, na.rm = TRUE),
    humidity = mean(humidity, na.rm = TRUE)
  ) %>%
  ungroup()


#### Join ####

df <- mol %>%
  full_join(
    clin %>% mutate(room = "Waiting room"),
    by = c("date", "daytime", "room")
  ) %>%
  full_join(pers_time, by = c("date", "daytime", "room")) %>%
  full_join(vent, by = c("date", "daytime", "room")) %>%
  full_join(temp_humid, by = c("date", "daytime", "room"))


#### Save ####

saveRDS(df, "data-clean/2019/combined-data.rds")
