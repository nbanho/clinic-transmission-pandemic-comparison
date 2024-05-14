#### Libraries ####

library(tidyverse)
library(lubridate)
library(haven)

#### Backup data ####

files <- list.files("data-raw/2021/environmental/backup/", full.names = TRUE)

masi2021 <- do.call(
  rbind,
  lapply(files, function(f) {
    read.table(f, sep = "\t", header = F, skip = 33) %>%
      set_names(c("date", "time", "humidity", "temperature", "co2", "ext")) %>%
      dplyr::select(-ext) %>%
      mutate(file = basename(f))
  })
) %>%
  mutate(
    date_created = as.Date(
      stringi::stri_extract(file, regex = "\\d{8}"), "%Y%m%d"
    ),
    date = as.Date(date, format = "%m/%d/%Y"),
    sensor = as.integer(gsub(
      "-", "",
      stringi::stri_extract(file, regex = "-\\d{1}")
    )),
    date_time = parse_date_time(paste(date, time),
      "%Y-%m-%d %I:%M:%S %p",
      tz = "CET"
    ),
    date_time = round_date(date_time, unit = "minutes"),
    date = as.Date(date_time)
  ) %>%
  filter(sensor > 1) %>%
  mutate(
    room = ifelse(sensor == 2, "Waiting room",
      ifelse(sensor == 3, "Corridor", "TB room")
    ),
    room = factor(room, levels = c("Waiting room", "Corridor", "TB room"))
  ) %>%
  dplyr::select(date, date_time, room, co2, temperature, humidity) %>%
  mutate(across(c(co2, temperature, humidity), as.numeric)) %>%
  filter(between(hour(date_time), 8, 15)) %>%
  mutate(co2 = ifelse(co2 <= 300, NA, co2))

#### Preprocessed data from Kathrin ####

masi2021_prep <- read_dta("data-raw/2021/environmental/co2_data.dta") %>%
  rename(
    date = Date,
    co2 = CO2,
    temperature = Temperature,
    humidity = Humidity,
    room = location
  ) %>%
  filter(room > 1) %>%
  mutate(
    room = ifelse(room == 2, "Waiting room", "TB room"),
    room = factor(room, levels = c("Waiting room", "TB room")),
    date_time = as.POSIXct(paste(date, format(new_time, format = "%H:%M:%S"))),
    across(c(co2, temperature, humidity), as.numeric)
  ) %>%
  dplyr::select(date, date_time, room, co2, temperature, humidity) %>%
  filter(between(hour(date_time), 8, 15))

#### Filter ####

# backup data
masi2021 %>%
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~date, scales = "free_x") +
  geom_line()

masi2021 <- masi2021 %>%
  filter(date %in% paste0("2021-", c(
    "10-11", "10-12", "10-13", "10-14", "10-15",
    "10-25", "10-26", "10-27", "10-28", "10-29",
    "11-04", "11-05", "11-06", "11-07", "11-08",
    "11-09", "11-10", "11-11", "11-12",
    "11-15", "11-16", "11-17", "11-18"
  )))

# prep data from Kathrin
masi2021_prep %>%
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~date, scales = "free_x") +
  geom_line()

masi2021_prep <- masi2021_prep %>%
  filter(date %in% paste0("2021-", c(
    "10-25", "10-26", "10-27", "10-28", "10-29",
    "11-04", "11-05", "11-08", "11-09", "11-10",
    "11-11", "11-12",
    "11-15", "11-16", "11-17", "11-18", "11-19"
  )))


#### Combine data ####

# join data
df <- full_join(
  masi2021,
  masi2021_prep,
  by = c("date", "date_time", "room")
)

df %>%
  filter(room == "Waiting room") %>%
  ggplot(aes(x = date_time)) +
  geom_line(aes(y = co2.x, color = "red")) +
  geom_line(aes(y = co2.y, color = "blue")) +
  facet_wrap(~date, scales = "free_x")

df %>%
  filter(room == "Waiting room") %>%
  ggplot(aes(x = date_time)) +
  geom_line(aes(y = temperature.x, color = "red")) +
  geom_line(aes(y = temperature.y, color = "blue")) +
  facet_wrap(~date, scales = "free_x")

df %>%
  filter(room == "Waiting room") %>%
  ggplot(aes(x = date_time)) +
  geom_line(aes(y = humidity.x, color = "red")) +
  geom_line(aes(y = humidity.y, color = "blue")) +
  facet_wrap(~date, scales = "free_x")

# coalesce data
imp_xy <- function(x, y) {
  if (!is.na(x) & !is.na(y)) {
    return(mean(c(x, y)))
  } else if (!is.na(x)) {
    return(x)
  } else if (!is.na(y)) {
    return(y)
  } else {
    return(NA)
  }
}

df <- df %>%
  mutate(
    co2 = map2_dbl(co2.x, co2.y, imp_xy),
    temperature = map2_dbl(temperature.x, temperature.y, imp_xy),
    humidity = map2_dbl(humidity.x, humidity.y, imp_xy)
  )

df %>%
  filter(room == "Waiting room") %>%
  ggplot(aes(x = date_time)) +
  geom_line(aes(y = co2.x, color = "red")) +
  geom_line(aes(y = co2.y, color = "blue")) +
  geom_line(aes(y = co2, color = "green")) +
  facet_wrap(~date, scales = "free_x")

df <- df %>%
  group_by(date, date_time, room) %>%
  summarise(
    co2 = mean(co2, na.rm = TRUE),
    temperature = mean(temperature, na.rm = TRUE),
    humidity = mean(humidity, na.rm = TRUE)
  ) %>%
  ungroup()

# save data
saveRDS(df, "data-clean/2021/environmental.rds")
