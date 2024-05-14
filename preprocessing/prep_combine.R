#### Libraries ####

library(tidyverse)
source("utils/trans_risk.r")

dim_wr <- c(10.55, 5.7, 3)
vol_wr <- prod(dim_wr)

comb_df <- tibble()
for (y in c(2019, 2021)) {
  #### Data ####

  # raw data
  path <- paste0("data-clean/", y, "/")
  mol <- readRDS(paste0(path, "molecular.rds"))
  clin <- readRDS(paste0(path, "clinical-count.rds"))
  env <- readRDS(paste0(path, "environmental.rds"))
  nppl <- readRDS(paste0(path, "tracking-no_of_people.rds"))
  ptime <- readRDS(paste0(path, "tracking-person_time.rds"))

  #### Preprocessing ####

  # filter data to waiting room
  mol <- mol %>%
    filter(room == "Waiting room") %>%
    select(-room)
  env <- env %>%
    filter(room == "Waiting room") %>%
    select(-room) %>%
    mutate(daytime = ifelse(hour(date_time) < 11, "Morning", "Afternoon"))
  nppl <- nppl %>%
    ungroup()
  ptime <- ptime %>%
    arrange(date, hr) %>%
    pivot_wider(
      names_from = hr,
      values_from = pt,
      names_prefix = "pt_hr"
    )

  # use mean temperature and humidity
  temp_rH <- env %>%
    group_by(date, daytime) %>%
    summarise(
      temperature = mean(temperature, na.rm = TRUE),
      humidity = mean(humidity, na.rm = TRUE)
    ) %>%
    ungroup()

  # compute AER
  ach <- env %>%
    dplyr::select(date, daytime, date_time, co2) %>%
    left_join(
      nppl,
      by = c("date", "date_time")
    ) %>%
    nest(data = c(date_time, co2, n)) %>%
    mutate(aer = sapply(data, function(x) {
      y <- x %>%
        arrange(date_time) %>%
        rename(C = co2) %>%
        mutate(
          C1 = lead(C),
          V = vol_wr,
        ) %>%
        na.omit()
      if (nrow(y) <= 60) {
        return(NA)
      } else {
        return(estimate_aer(y))
      }
    })) %>%
    dplyr::select(date, daytime, aer)

  #### Combine data ####

  # join all data
  df <- full_join(mol, clin, by = c("date", "daytime")) %>%
    full_join(ach, by = c("date", "daytime")) %>%
    full_join(temp_rH, by = c("date", "daytime")) %>%
    full_join(ptime, by = c("date"))

  # filter if all or only clin is not na
  df <- df %>%
    filter(
      !is.na(diagnosed)
    ) %>%
    rowwise() %>%
    filter(
      !all(is.na(c_across(matches("dna_copies|temperature|humidity|aer|pt_hr"))))
    ) %>%
    ungroup()

  # append
  comb_df <- rbind(
    comb_df,
    df %>% mutate(year = y)
  )
}

#### Fine tuning ####

# minor edits
comb_df <- comb_df %>%
  mutate(date = as.Date(date)) %>%
  arrange(year, date, daytime) %>%
  select(
    year,
    date,
    daytime,
    dna_copies, sampling_time,
    diagnosed, undiagnosed, uninfectious, registered,
    aer, temperature, humidity,
    matches("pt_hr")
  ) %>%
  mutate(sampling_time = ifelse(!is.na(sampling_time), sampling_time,
    ifelse(year == 2021, 3, 2)
  ))

# remove daytimes of missing mol when noone was in clinic
comb_df %>%
  filter(is.na(dna_copies)) %>%
  View()

no_ppl_21 <- readRDS("data-clean/2021/tracking-no_of_people.rds")

no_ppl_21 %>%
  ggplot(aes(x = date_time, y = n)) +
  geom_line() +
  facet_wrap(~date, scales = "free_x")

co2_21 <- readRDS("data-clean/2021/environmental.rds")

co2_21 %>%
  filter(room == "Waiting room") %>%
  ggplot(aes(x = date_time, y = co2)) +
  geom_line() +
  facet_wrap(~date, scales = "free_x")

no_ppl_19 <- readRDS("data-clean/2019/tracking-no_of_people.rds")

no_ppl_19 %>%
  ggplot(aes(x = date_time, y = n)) +
  geom_line() +
  facet_wrap(~date, scales = "free_x")


comb_df <- comb_df %>%
  filter(
    !(date == "2021-10-15" & daytime == "Afternoon"),
  )

saveRDS(comb_df, "data-clean/combined-data-wr.rds")

comb_df %>%
  group_by(year) %>%
  summarise(
    all = n(),
    non_missing = sum(!is.na(dna_copies)),
    all_days = length(unique(date)),
    non_missing_days = length(unique(date[!is.na(dna_copies)]))
  ) %>%
  View()
