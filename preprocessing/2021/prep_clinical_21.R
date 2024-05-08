#### Libraries ####

library(tidyverse)
library(lubridate)

#### Data ####

# load clinical data
clin_pat_time_masi <- read.csv("data-raw/2021/clinical/patient-time.csv") %>%
  rename(date = action_date) %>%
  mutate(
    time = parse_date_time(
      paste(date, time),
      "%m/%d/%Y %I:%M:%S %p",
      tz = "CET"
    ),
    date = as.Date(time)
  ) %>%
  filter(!is.na(time))

clin_tb_pat <- readxl::read_excel(
  "data-raw/2021/clinical/CocT Masiphumelele_clinic data Apr_Nov 2021.xlsx",
  sheet = "Worksheet 1"
) %>%
  rename(
    age = `Age(Years)`,
    gender = Gender,
    tb_treat_start = `Treatment Start Date`,
    folder_number = `Folder Number`
  ) %>%
  dplyr::select(age, gender, tb_treat_start, folder_number) %>%
  mutate(
    gender = factor(gender, levels = c("Male", "Female")),
    tb_treat_start = as.Date(tb_treat_start),
    folder_number = as.integer(folder_number)
  )

clin_tb_sus <- read.csv("data-raw/2021/clinical/tb-suspects.csv") %>%
  mutate(
    was_suspected = TRUE,
    sus_tested_positive = ifelse(
      result_txt %in% c(
        "TB_GENEXP_POS_RIF_S",
        "TB_GENEXP_POS_RIF_R"
      ),
      TRUE, FALSE
    ),
    sus_started_treat = ifelse(
      care_status_txt %in% c(
        "CARE_STATUS_STARTED",
        "CARE_STATUS_STARTED_OTHER"
      ),
      TRUE, FALSE
    ),
    sus_treat_start = ifelse(sus_started_treat, care_status_date, NA),
    sus_treat_start = as.Date(sus_treat_start, format = "%d.%m.%y")
  ) %>%
  dplyr::select(
    patient_id,
    was_suspected,
    sus_tested_positive,
    sus_started_treat,
    sus_treat_start
  )

# combine clinical data and determine infectivity
clinical <- clin_pat_time_masi %>%
  left_join(clin_tb_pat) %>%
  left_join(clin_tb_sus) %>%
  rename(date_time = time) %>%
  mutate(
    diff_treat_start_1 = as.numeric(date - tb_treat_start),
    diff_treat_start_2 = as.numeric(date - sus_treat_start),
    is_infectious_1 = ifelse(diff_treat_start_1 <= 28, TRUE, FALSE),
    is_infectious_2 = ifelse(diff_treat_start_2 <= 28, TRUE, FALSE),
    is_infectious = is_infectious_1 | is_infectious_2,
    is_uninfectious = !is_infectious
  ) %>%
  mutate(
    is_undiagnosed = ifelse(is.na(is_infectious), TRUE, FALSE),
    is_infectious = ifelse(is.na(is_infectious), FALSE, is_infectious),
    is_uninfectious = ifelse(is.na(is_uninfectious), FALSE, is_uninfectious)
  ) %>%
  dplyr::select(
    patient_id,
    date,
    date_time,
    is_infectious,
    is_undiagnosed,
    is_uninfectious
  )

# inspect
clinical %>%
  group_by(patient_id) %>%
  filter(any(is_infectious)) %>%
  arrange(patient_id, date) %>%
  View()

#### Summarize ####

# count number of patients
clin_count <- clinical %>%
  filter(between(hour(date_time), 8, 14)) %>%
  mutate(daytime = ifelse(hour(date_time) < 11, "Morning", "Afternoon")) %>%
  group_by(date, daytime) %>%
  summarise(
    diagnosed = n_distinct(patient_id[is_infectious]),
    undiagnosed = n_distinct(patient_id[is_undiagnosed]),
    uninfectious = n_distinct(patient_id[is_uninfectious])
  ) %>%
  mutate(
    registered = diagnosed + undiagnosed + uninfectious
  ) %>%
  ungroup()

# table summarize
clin_count %>%
  summarise(across(c(diagnosed, undiagnosed, uninfectious, registered), mean))

clin_count %>% filter(
  date %in% c(
    "2021-10-13",
    "2021-10-15",
    "2021-10-25",
    "2021-11-04",
    "2021-11-05"
  )
)

# save data
saveRDS(clin_count, "data-clean/2021/clinical.rds")
