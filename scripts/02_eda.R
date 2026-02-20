library(tidyverse)

complete <- read_csv("data/processed/complete_14-25.csv", show_col_types = FALSE)

big_6 <- c("Arsenal", "Chelsea", "Liverpool", "Manchester City", "Manchester United", "Tottenham Hotspur")

complete_final <- complete |>
  group_by(season) |>
  mutate(
    relative_annual_wages = annual_wages_gbp / median(annual_wages_gbp, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(big_6 = if_else(team %in% big_6, "Big 6", "Other"))

x_max_other <- complete_final |> filter(big_6 == "Other") |> pull(relative_annual_wages) |> max(na.rm = TRUE)
x_min_big6 <- complete_final |> filter(big_6 == "Big 6") |> pull(relative_annual_wages) |> min(na.rm = TRUE)

p <- ggplot(complete_final, aes(x = relative_annual_wages, y = points, color = big_6)) +
  geom_vline(xintercept = x_min_big6, linetype = "dashed") +
  geom_vline(xintercept = x_max_other, linetype = "dashed") +
  geom_point() +
  labs(
    x = "Relative annual payroll (vs season median)",
    y = "Points",
    title = "Points vs relative payroll, Premier League 2014–2025",
    color = NULL
  ) +
  theme_minimal()

dir.create("plots", showWarnings = FALSE)
ggsave("plots/points_vs_relative_payroll.png", plot = p)
