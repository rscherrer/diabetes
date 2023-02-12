rm(list = ls())

library(tidyverse)

data <- read_csv("diabetes2.csv")
data$date <- as.Date(data$date)

data %>%
  group_by(entry, time) %>%
  filter(key == "glyc") %>%
  summarize(
    dglyc =
  )

data <- data %>%
  arrange(date, time) %>%
  group_by(entry, date, key) %>%
  nest() %>%
  pivot_wider(names_from = key, values_from = data)

data %>%
  ungroup() %>%
  mutate(dglyc = map(glyc, ~ last(.x) - first(.x)))
