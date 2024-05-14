#### Libraries ####

library(tidyverse)
library(lubridate)
library(zoo)

lin_imp <- function(x, mg = 600) {
  na.locf(
    na.locf(
      na.approx(x, maxgap = mg, na.rm = FALSE),
      maxgap = mg, na.rm = FALSE
    ),
    fromLast = TRUE, maxgap = mg, na.rm = FALSE
  )
}


#### Data ####

# files
tracking_files <- list.files(
  "data-raw/2019/tracking/annotated/",
  full.names = TRUE
)

# load tracking data
tracking_data <- do.call(
  rbind, parallel::mclapply(tracking_files, readRDS, mc.cores = 6)
) %>%
  mutate(date = as.Date(date_time)) %>%
  filter(
    is_waitingroom,
    between(hour(date_time), 8, 15)
  )


#### No. of people ####

no_ppl <- data.frame(date = unique(tracking_data$date))
no_ppl$date_time <- lapply(no_ppl$date, function(d) {
  ts <- as.POSIXct(paste(d, "08:00:00"))
  te <- as.POSIXct(paste(d, "16:00:00"))
  t <- seq(ts, te, by = "1 sec")
  return(t)
})
no_ppl <- unnest(no_ppl, date_time)
no_ppl <- no_ppl %>%
  left_join(
    tracking_data %>%
      dplyr::select(date_time, obs_id),
    by = "date_time"
  ) %>%
  group_by(date, date_time) %>%
  summarise(
    n = ifelse(all(is.na(obs_id)), NA, n_distinct(obs_id, na.rm = TRUE))
  ) %>%
  ungroup()

no_ppl_min <- no_ppl %>%
  mutate(date_time = round_date(date_time, "minute")) %>%
  group_by(date, date_time) %>%
  summarise(
    n = ifelse(all(is.na(n)), NA, mean(n, na.rm = TRUE))
  ) %>%
  ungroup()

# plot no of people
no_ppl_min %>%
  ggplot(aes(x = date_time, y = n)) +
  geom_line() +
  facet_wrap(~date, scales = "free_x")

# imputation
no_ppl_min <- no_ppl_min %>%
  group_by(date) %>%
  mutate(n = lin_imp(n, mg = 40)) %>%
  ungroup()

no_ppl_min %>%
  ggplot(aes(x = date_time, y = n)) +
  geom_line() +
  facet_wrap(~date, scales = "free_x")

# save
saveRDS(no_ppl, "data-clean/2019/tracking-no_of_people.rds")


#### Person-time ####

# imputation
no_ppl_imp <- no_ppl %>%
  mutate(hr = hour(date_time)) %>%
  group_by(date, hr) %>%
  arrange(date_time) %>%
  mutate(n = lin_imp(n, mg = 40 * 60)) %>%
  ungroup()

# plot no of people
no_ppl_imp %>%
  ggplot(aes(x = date_time, y = n)) +
  geom_line() +
  facet_wrap(~date, scales = "free_x")

# compute person-time by daytime filtered by Mtb sampling
pers_time <- no_ppl_imp %>%
  group_by(date, hr) %>%
  summarise(
    pt = sum(n) / 3600
  ) %>%
  ungroup() %>%
  filter(between(hr, 8, 15))

# save
saveRDS(pers_time, "data-clean/2019/tracking-person_time.rds")
