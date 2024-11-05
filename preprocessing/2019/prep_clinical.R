library(tidyverse)
library(readxl)

dates <- readRDS("data-clean/combined-data-wr.rds") %>%
  group_by(year) %>%
  summarise(min_date = min(date), max_date = max(date)) %>%
  ungroup()

tb_ident <- read_xlsx(
  "data-raw/Copy of TB Identification and Treatment Masiphumelele 2019 and 2021.xlsx",
  sheet = 1,
  skip = 6
) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(
    between(
      Date,
      dates$min_date[dates$year == 2019],
      dates$max_date[dates$year == 2019]
    ) |
      between(
        Date,
        dates$min_date[dates$year == 2021],
        dates$max_date[dates$year == 2021]
      )
  ) %>%
  mutate(year = lubridate::year(Date))

csr <- tb_ident %>%
  group_by(year) %>%
  summarise(
    suspected = n(),
    cases = sum(!is.na(`Date RX started`))
  ) %>%
  ungroup()

xpert <- tb_ident %>%
  mutate(across(c(`Test name`, `Test result`), tolower)) %>%
  filter(grepl("xpert", `Test name`)) %>%
  group_by(year, `Episode id`) %>%
  summarise(xpert_pos = any(grepl("pos", `Test result`))) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(
    xpert_pos = sum(xpert_pos),
    xpert_tests = n(),
  ) %>%
  ungroup() %>%
  mutate(
    xpert_neg = xpert_tests - xpert_pos
  )

tb_char <- read_xlsx(
  "data-raw/Copy of TB Identification and Treatment Masiphumelele 2019 and 2021.xlsx",
  sheet = 2,
  skip = 6
) %>%
  mutate(
    date = `Treatment Start Date`,
    date = as.Date(date)
  ) %>%
  filter(
    between(
      date,
      dates$min_date[dates$year == 2019],
      dates$max_date[dates$year == 2019]
    ) |
      between(
        date,
        dates$min_date[dates$year == 2021],
        dates$max_date[dates$year == 2021]
      )
  ) %>%
  mutate(year = lubridate::year(date))

hiv_status <- tb_char %>%
  mutate(hiv = tolower(`HIV Status`)) %>%
  group_by(year) %>%
  summarise(
    hiv_pos = sum(grepl("pos|+", hiv), na.rm = TRUE),
    hiv_neg = sum(grepl("neg|-", hiv), na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    hiv_pos_ratio = hiv_pos / (hiv_pos + hiv_neg)
  )

mdr_cases <- tb_char %>%
  mutate(
    outcome = paste(
      `User Entered Outcome`, `System Generated Outcome`
    ),
    outcome = tolower(outcome)
  ) %>%
  group_by(year) %>%
  summarise(
    mdr = sum(grepl("mdr", outcome))
  ) %>%
  ungroup()

smear <- tb_char %>%
  rename(
    smear_result = `Smear Test Result(Pre/Treatment)01`,
    smear_grading = `Smear Grading(Pre/Treatment)01`
  ) %>%
  mutate(
    smear_result = tolower(smear_result),
    smear_grading = tolower(smear_grading),
    smear_result = case_when(
      grepl("pos", smear_result) ~ "Positive",
      grepl("scant", smear_result) | grepl("scant", smear_grading) ~ "Scanty",
      grepl("neg", smear_result) ~ "Negative"
    ),
    smear_result = ifelse(is.na(smear_result), "No smear", smear_result),
    smear = ifelse(
      smear_result == "Positive",
      paste0("Smear / Positive", gsub("[a-z]", "", ifelse(is.na(smear_grading), "", smear_grading))),
      ifelse(smear_result == "Scanty",
        "Smear / Scanty",
        ifelse(smear_result == "Negative", "Negative", "No smear")
      )
    )
  ) %>%
  group_by(year, smear) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  tidyr::complete(year, smear) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  spread(smear, n) %>%
  mutate(
    Positive = `Smear / Positive` +
      `Smear / Positive+` +
      `Smear / Positive++` +
      `Smear / Positive+++`,
    `Smear / Scanty` = 0
  )

relapse <- tb_char %>%
  rename(category = `Patient Category`) %>%
  mutate(category = tolower(category)) %>%
  group_by(year) %>%
  summarise(
    relapses = sum(category == "relapse"),
    tb_patients = n()
  ) %>%
  ungroup() %>%
  mutate(
    relapse_ratio = relapses / tb_patients
  )

clinical_data <- csr %>%
  left_join(hiv_status, by = "year") %>%
  left_join(mdr_cases, by = "year") %>%
  left_join(smear, by = "year") %>%
  left_join(xpert, by = "year") %>%
  left_join(relapse, by = "year")

write.csv(
  clinical_data,
  file = "data-clean/clinical-data.csv",
  row.names = FALSE
)
