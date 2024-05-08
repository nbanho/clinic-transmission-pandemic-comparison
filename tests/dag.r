library(mediation)
library(tidyverse)

set.seed(1234)

# sample size
n <- 10000


# data
# df <- data.frame(
#   pandemic = c(rep(0, n / 2), rep(1, n / 2)),
#   co2 = c(rnorm(n / 2, 2000, 100), rnorm(n / 2, 1000, 100)),
#   no_people = c(rpois(n / 2, 40), rpois(n / 2, 30)),
#   humid = c(runif(n / 2, 50, 80), runif(n / 2, 40, 60)),
#   error = rnorm(n)
# ) %>%
#   mutate(vent_rate = 0.004 / ((co2 - 400) / 1000),
#          ach = 3600 * vent_rate * no_people / 240) %>%
#   mutate(mtb = 2 * no_people - 1 * ach - 4 * pandemic + 0.5 * humid + error)

df <- data.frame(
  pandemic = c(rep(0, n / 2), rep(1, n / 2)),
  no_people = c(rnorm(n / 2, 10, 1), rnorm(n / 2, 2, 1)),
  humid = c(rnorm(n / 2, 5, 1), rnorm(n / 2, 0, 1)),
  error = rnorm(n)
) %>%
  mutate(
    ach = 0.25 * no_people + c(rnorm(n / 2, 0.5, 1), rnorm(n / 2, -0.5, 1)),
    mtb = 2 * no_people - 1 * ach - 4 * pandemic + 0.5 * humid
  )

cor(df$ach, df$no_people) 
plot(df$ach, df$no_people)

# effect of mask wearing
m1 <- lm(mtb ~ pandemic + no_people + ach + humid + error, data = df)
summary(m1)
coef(m1)['pandemic'] # b2 = -4

# effect of fixed appointments and patient triage 
coef(lm(no_people ~ pandemic, data = df))['pandemic'] # a1 = -10.42
coef(m3)['no_people'] # b1 = 2
# a1 * b1 = -20.84
coef(lm(ach ~ no_people, data = df))['no_people'] # a2 = -0.13
coef(m3)['ach'] # b3 = -1
# a1 * a2 * b3 = -1.33

# effect of natural ventilation
coef(lm(ach ~ pandemic, data = df))['pandemic'] # a3 = 0.55
coef(m1)['ach'] # b3 = -1
coef(lm(humid ~ pandemic, data = df))['pandemic'] # a4 = -14
coef(m1)['humid'] # b4 = 0.5
# a3 x b3 + a4 x b4 = -0.55 + (-7) = -7.55

# confirm
te1 <- coef(m1)['pandemic'] + 
  coef(lm(no_people ~ pandemic, data = df))['pandemic'] * coef(m3)['no_people'] + 
  coef(lm(no_people ~ pandemic, data = df))['pandemic'] * coef(lm(ach ~ no_people, data = df))['no_people'] * coef(m3)['ach'] +
  coef(lm(ach ~ pandemic, data = df))['pandemic'] * coef(m1)['ach'] +
  coef(lm(humid ~ pandemic, data = df))['pandemic'] * coef(m1)['humid']
te2 <- coef(m1)['pandemic'] + 
  coef(lm(no_people ~ pandemic, data = df))['pandemic'] * coef(m3)['no_people'] + 
  coef(lm(ach ~ pandemic, data = df))['pandemic'] * coef(m1)['ach'] +
  coef(lm(humid ~ pandemic, data = df))['pandemic'] * coef(m1)['humid']
true_te <- mean(df$mtb[(n/2+1):n]) - mean(df$mtb[1:(n/2)])

cat(paste("Total effect with a1 * a2 * b3: ", round(te1, 2), 
       "\nTotal effect without a1 * a2 * b3: ", round(te2, 2),
       "\nTrue effect: ", round(true_te, 2)))
