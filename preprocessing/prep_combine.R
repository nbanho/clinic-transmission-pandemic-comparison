#### Libraries ####

library(tidyverse)

comb_df <- tibble()
for (y in c(2019, 2021)) {
  #### Data ####

  # raw data
  path <- paste0("data-clean/", y, "/")
  mol <- readRDS(paste0(path, "molecular.rds"))
  clin <- readRDS(paste0(path, "clinical.rds"))
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
    select(-room)
  nppl <- nppl %>%
    filter(room == "Waiting room") %>%
    select(-room)
  ptime <- ptime %>%
    filter(room == "Waiting room") %>%
    select(-room)

  # use maximum CO2 level and number of people in the room
  # one exception: set NA the #people for November 17 (peak probably missing)
  # for temperature and humidity, use the mean
  env <- env %>%
    group_by(date) %>%
    summarise(
      co2 = max(co2, na.rm = TRUE),
      temperature = mean(temperature, na.rm = TRUE),
      humidity = mean(humidity, na.rm = TRUE)
    )
  nppl <- nppl %>%
    group_by(date) %>%
    summarise(
      np = max(n, na.rm = TRUE)
    ) %>%
    mutate(np = ifelse(date == as.Date("2021-11-17"), NA, np))

  # wide format for person time
  ptime <- ptime %>%
    pivot_wider(names_from = h, values_from = pt, names_prefix = "pt_h")

  #### Combine data ####

  # join all data
  df <- full_join(mol, clin, by = c("date", "daytime")) %>%
    full_join(env, by = "date") %>%
    full_join(nppl, by = "date") %>%
    full_join(ptime, by = "date")

  # filter if all or only clin is not na
  df <- df %>%
    filter(
      !is.na(diagnosed)
    ) %>%
    rowwise() %>%
    filter(
      !all(is.na(c_across(matches("pt_"))), is.na(dna_copies), is.na(co2))
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
  arrange(year, date) %>%
  select(
    year,
    date,
    daytime,
    dna_copies, sampling_time,
    diagnosed, undiagnosed, uninfectious, registered,
    co2, temperature, humidity,
    np, matches("pt_")
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
  ggplot(aes(x = date_time, y = n, color = room)) +
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
  ggplot(aes(x = date_time, y = n, color = room)) +
  geom_line() +
  facet_wrap(~date, scales = "free_x")


comb_df <- comb_df %>%
  filter(
    !(date == "2021-10-15" & daytime == "Afternoon"),
  )

# sum person time from 8am to 11am and from 11am to 2pm
# ignore person-time of 2-4pm
comb_df <- comb_df %>%
  mutate(
    pt_morning = rowSums(select(., pt_h8, pt_h9, pt_h10)),
    pt_afternoon = rowSums(select(., pt_h11, pt_h12, pt_h13)),
    pt = ifelse(daytime == "Morning", pt_morning, pt_afternoon)
  ) %>%
  select(-pt_morning, -pt_afternoon, -matches("pt_"))

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
