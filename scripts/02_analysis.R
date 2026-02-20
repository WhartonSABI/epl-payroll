install.packages("tidyverse")
library(tidyverse)
library(broom)

# read raw data

complete <- read_csv("complete_14-25.csv", show_col_types = FALSE)

# median dataframe (unused)
season_medians <- complete |>
  group_by(season) |>
  summarize(median_wage = median(annual_wages_gbp, na.rm = TRUE))


# converting to relative payroll
complete_final <- complete |>
  group_by(season) |>
  mutate(
    relative_annual_wages = annual_wages_gbp / median(annual_wages_gbp, na.rm = TRUE)
  ) |>
  ungroup()

# regression 1: E[points] vs relative wages
regression_1 <- lm(points ~ relative_annual_wages, data = complete_final)

ggplot(data = complete_final, mapping = aes(x= relative_annual_wages, y = points)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Points vs Relative Annual Wages in the Premier League, 2014-2025"
  )
  
  
# regression 2: E[Points] vs wages, team as factor
regression_2 <- lm(points ~ relative_annual_wages + team, data = complete_final)

ggplot(data = complete_final, mapping = aes(x = relative_annual_wages, 
                                             y = points, color = team)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(xlim = c(0, 4), ylim = c(0,100))

# regression 3: simple E[wages] vs points
regression_3 <- lm(relative_annual_wages ~ points, data = complete_final)

ggplot(data = complete_final, mapping = aes(
  x = points, y = relative_annual_wages
  )) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


# regression 4: E[wages] vs points, team as factor
regression_4 <- lm(relative_annual_wages ~ points + team, data = complete_final)


outputs_1 <- tidy(regression_1)
outputs_2 <- tidy(regression_2)
outputs_3 <- tidy(regression_3)
outputs_4 <- tidy(regression_4)
