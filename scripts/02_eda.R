source("scripts/00_common.R")

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Missing required R package: ggplot2. Install it before running this script.", call. = FALSE)
}

library(ggplot2)

complete <- read_complete_data()
complete_final <- add_payroll_features(complete)

x_max_other <- complete_final |>
  filter(big_six_group == "Other") |>
  pull(relative_annual_wages) |>
  max(na.rm = TRUE)

x_min_big_six <- complete_final |>
  filter(big_six_group == "Big Six") |>
  pull(relative_annual_wages) |>
  min(na.rm = TRUE)

p <- ggplot(complete_final, aes(x = relative_annual_wages, y = points, color = big_six_group)) +
  geom_vline(xintercept = x_min_big_six, linetype = "dashed") +
  geom_vline(xintercept = x_max_other, linetype = "dashed") +
  geom_point() +
  labs(
    x = "Relative annual payroll (vs season median)",
    y = "Points",
    title = "Points vs relative payroll, Premier League 2013-14 through 2024-25",
    color = NULL
  ) +
  theme_minimal()

dir.create("plots", showWarnings = FALSE)
ggsave("plots/points_vs_relative_payroll.png", plot = p, width = 7, height = 5, dpi = 300)

message("Wrote plots/points_vs_relative_payroll.png")
