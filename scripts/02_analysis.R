library(tidyverse)

# Converting to relative annual wages (dividing by median)
complete_add_relative <- complete |>
  group_by(season) |>
  mutate(
    relative_annual_wages = annual_wages_gbp / median(annual_wages_gbp, na.rm = TRUE)
  ) |>
  ungroup()

# Regression #1: Simple regression, points vs relative wages
naive_regression <- lm(points ~ relative_annual_wages, data = complete_add_relative)
# Plotting data and regression line
ggplot(data = complete_add_relative, mapping = aes(x= relative_annual_wages, y = points)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Points vs Relative Annual Wages in the Premier League, 2014-2025"
  )
