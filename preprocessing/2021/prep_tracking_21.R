#### Libraries ####

library(tidyverse)
library(lubridate)
library(imputeTS)


#### Data ####

# files
tracking_files <- list.files(
  "data-raw/2021/tracking/annotated/",
  full.names = TRUE
)

# load tracking data
tracking_data <- do.call(
  rbind, parallel::mclapply(tracking_files, readRDS, mc.cores = 6)
) %>%
  rename(date_time = time) %>%
  mutate(date = as.Date(date_time)) %>%
  filter(between(hour(date_time), 8, 15))

#### No. of people ####

no_ppl <- data.frame(date = unique(tracking_data$date))
no_ppl$counts <- lapply(no_ppl$date, function(d) {
  ts <- as.POSIXct(paste(d, "08:00:00"))
  te <- as.POSIXct(paste(d, "16:00:00"))
  t <- seq(ts, te, by = "1 min")
  cn <- data.frame(date_time = t)
  cn$wr <- unlist(parallel::mclapply(cn$date_time, function(dt) {
    sub <- tracking_data %>%
      filter(between(date_time, dt, dt + minutes(1))) %>%
      filter(is_waitingroom)
    return(n_distinct(sub$obs_id))
  }, mc.cores = 6))
  cn$cr <- unlist(parallel::mclapply(cn$date_time, function(dt) {
    sub <- tracking_data %>%
      filter(between(date_time, dt, dt + minutes(1))) %>%
      filter(is_passage)
    return(n_distinct(sub$obs_id))
  }, mc.cores = 6))
  return(cn)
})

no_ppl <- no_ppl %>%
  unnest(counts) %>%
  reshape2::melt(c("date", "date_time")) %>%
  rename(room = variable, n = value) %>%
  mutate(room = ifelse(room == "wr", "Waiting room", "Corridor"))

# plot no of people
no_ppl %>%
  ggplot(aes(x = date_time, y = n, color = room)) +
  geom_line() +
  facet_wrap(~date, scales = "free_x")

# remove trailing and leading zeros
remove_zeros <- function(df) {
  n <- df$n
  if (all(n == 0)) {
    return(df)
  } else {
    first_non_zero <- which(n != 0)[1]
    last_non_zero <- length(n) - which(rev(n) != 0)[1] + 1
    df <- df[first_non_zero:last_non_zero, ]
    return(df)
  }
}

no_ppl_filt <- no_ppl %>%
  group_by(date, room) %>%
  arrange(date_time) %>%
  nest(data = c(date_time, n)) %>%
  mutate(data = map(data, remove_zeros)) %>%
  unnest(data)

# strict daytime filter
no_ppl_filt <- no_ppl_filt %>%
  filter(
    !(date == "2021-10-14" & hour(date_time) < 11),
    !(date == "2021-10-15" & hour(date_time) >= 11),
    !(date == "2021-11-10" & hour(date_time) < 11),
    !(date == "2021-11-11" & hour(date_time) >= 11),
    !(date == "2021-11-17" & hour(date_time) < 11)
  )

no_ppl_filt %>%
  ggplot(aes(x = date_time, y = n, color = room)) +
  geom_line() +
  facet_wrap(~date, scales = "free_x")

# save
saveRDS(no_ppl_filt, "data-clean/2021/tracking-no_of_people.rds")

#### Person-time ####

# compute person-time
pers_time <- tracking_data %>%
  filter(is_waitingroom | is_passage) %>%
  mutate(
    room = ifelse(is_waitingroom, "Waiting room", "Corridor"),
    h = hour(date_time)
  ) %>%
  group_by(date, room, h, obs_id) %>%
  summarise(
    start_time = min(date_time),
    end_time = max(date_time)
  ) %>%
  mutate(pt = as.numeric(
    difftime(end_time, start_time, units = "hours")
  )) %>%
  group_by(date, h, room) %>%
  summarise(
    pt = sum(pt)
  ) %>%
  ungroup()

# plot
pers_time %>%
  filter(room == "Waiting room") %>%
  ggplot(aes(x = h, y = pt)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~date)

# save
saveRDS(pers_time, "data-clean/2021/tracking-person_time.rds")
