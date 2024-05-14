#### Libraries ####

library(tidyverse)
library(lubridate)
library(readxl)
library(reshape2)


#### Data ####

# load clinical data
clinical <- read_xlsx("data-raw/2019/clinical/Clinical_data29Jan20.xlsx") %>%
  dplyr::select(
    patient_id, patient_age_years, patient_gender,
    GeneXpert, event_updated_at, Start_Date, Treatment_Outcome_Date
  ) %>%
  rename(
    age = patient_age_years,
    gender = patient_gender,
    test_result = GeneXpert,
    date_time = event_updated_at,
    tb_treat_start = Start_Date,
    tb_treat_end = Treatment_Outcome_Date
  ) %>%
  mutate(
    date_time = as.POSIXct(date_time, tz = "CET"),
    date = as.Date(date_time),
    across(c(tb_treat_start, tb_treat_end), ~ as.Date(.x))
  )

#### Checks ####
#' look for people who tested negative or positive but did not initiate a TB treatment
#' and where not on a prior treatment
treatment_delay <- function(reg_date, test_result, treat_date) {
  if (!is.na(test_result)) {
    if (test_result == "NONE") {
      return(NA)
    } else {
      if (is.na(treat_date)) {
        return(-1) # no treatment initiated
      } else if (treat_date < reg_date) {
        return(0) # prior treatment
      } else {
        return(as.numeric(treat_date - reg_date)) # treatment delay
      }
    }
  } else {
    return(NA)
  }
}

clinical$treat_dt <- mapply(
  treatment_delay,
  reg_date = clinical$date,
  test_result = clinical$test_result,
  treat_date = clinical$tb_treat_start
)

clinical %>%
  filter(
    !is.na(treat_dt),
    treat_dt == -1
  )
# --> everyone with a test result was eventually treated

clinical %>%
  group_by(patient_id) %>%
  arrange(date_time) %>%
  filter(
    any(!is.na(treat_dt)),
    any(treat_dt > 0)
  )
# --> few patients with treatment delay

#' look for people who initiated treatment
#' although they did not have a prior test result

clinical %>%
  group_by(patient_id) %>%
  arrange(date_time) %>%
  filter(
    all(is.na(test_result)),
    all(!is.na(tb_treat_start)),
    tb_treat_start > date
  )

# --> one person started treatment during the study period without a GeneXpert test result

#### Preprocessing ####
#' We consider as infectious everyone who is on treatment
#' or will be on treatment later,
#' and we assume they stay infectious until 28 days into treatment.
#' We assume as non-infectious everyone who completed a treatment.

clinical <- clinical %>%
  mutate(
    is_infectious = ifelse(!is.na(tb_treat_start),
      ifelse(date <= tb_treat_start + days(28), TRUE, FALSE), FALSE
    ),
    is_uninfectious = ifelse(!is.na(tb_treat_start),
      ifelse(date > tb_treat_start, TRUE, FALSE), FALSE
    ),
    is_undiagnosed = ifelse(is.na(tb_treat_start), TRUE, FALSE)
  )

clinical %>%
  group_by(patient_id) %>%
  filter(any(is_infectious)) %>%
  arrange(patient_id, date) %>%
  View()

saveRDS(clinical, "data-clean/2019/clinical.rds")

#### Summarize ####

# count number of patients by daytime filtered by Mtb sampling
clin_count <- clinical %>%
  filter(between(hour(date_time), 8, 15)) %>%
  mutate(daytime = ifelse(hour(date_time) < 11, "Morning", "Afternoon")) %>%
  group_by(date, daytime) %>%
  summarise(
    diagnosed = sum(is_infectious),
    undiagnosed = sum(is_undiagnosed),
    uninfectious = sum(is_uninfectious)
  ) %>%
  mutate(
    registered = diagnosed + undiagnosed + uninfectious
  ) %>%
  ungroup()

# table summarize
clin_count %>%
  summarise(across(c(diagnosed, undiagnosed, uninfectious, registered), mean))

# save data
saveRDS(clin_count, "data-clean/2019/clinical-count.rds")
