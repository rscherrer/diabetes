rm(list = ls())

library(tidyverse)
library(rethinking)

theme_set(theme_classic())

# Load the data
diabetes_data <- read_csv("diabetes.csv")

# Modify / add some variables
diabetes_data <- diabetes_data %>%
  mutate(
    gly_change = gly_aft - gly_bef,
    ratio = insulin / glucose,
    ratio_s = (ratio - mean(ratio)) / sd(ratio),
    meal = fct_recode(meal, Breakfast = "B")
  )

# Useful metrics
mean_ratio <- mean(diabetes_data$ratio)
sd_ratio <- sd(diabetes_data$ratio)

# Fitted model
mod <- quap(
  flist = alist(
    gly_change ~ dnorm(mean = mu, sd = sigma),
    mu <- a + b * ratio_s,
    a ~ dnorm(mean = 0, sd = 10),
    b ~ dunif(min = -100, max = 0),
    sigma ~ dexp(rate = 10)
  ),
  data = diabetes_data
)

# Extract maximum a posteriori
map_a_s <- coef(mod)[["a"]]
map_b_s <- coef(mod)[["b"]]

# Unstandardize
map_a <- map_a_s - map_b_s * mean_ratio / sd_ratio
map_b <- map_b_s / sd_ratio

# Expected change in glycemia
expected_gly_change <- t(apply(link(mod), 2, PI, 0.89))
colnames(expected_gly_change) <- paste0(c("pi05", "pi94"), "_expected_gly_change")

diabetes_data <- diabetes_data %>% cbind(expected_gly_change)

# Predicted change through simulations
simulated_gly_change <- t(apply(sim(mod), 2, PI, 0.89))
colnames(simulated_gly_change) <- paste0(c("pi05", "pi94"), "_simulated_gly_change")

diabetes_data <- diabetes_data %>% cbind(simulated_gly_change)

# Plot the data and the model
diabetes_data %>%
  ggplot(aes(x = ratio, y = gly_change)) +
  geom_point() +
  geom_abline(intercept = map_a, slope = map_b) +
  geom_ribbon(
    aes(
      xmin = ratio, xmax = ratio,
      ymin = pi05_expected_gly_change,
      ymax = pi94_expected_gly_change
    ),
    alpha = 0.3
  ) +
  geom_ribbon(
    aes(
      xmin = ratio, xmax = ratio,
      ymin = pi05_simulated_gly_change,
      ymax = pi94_simulated_gly_change
    ),
    alpha = 0.1
  ) +
  facet_wrap(. ~ meal) +
  xlab("Insulin-glucose ratio (units/g)") +
  ylab("Change in glycemia (mg/dL)") +
  geom_hline(yintercept = 0, linetype = 2)

ggsave("results.png", width = 3, height = 2.5, dpi = 300)
