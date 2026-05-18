source("scripts/00_common.R")

required_plot_packages <- c("ggplot2", "scales")
missing_plot_packages <- required_plot_packages[
  !vapply(required_plot_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_plot_packages) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_plot_packages, collapse = ", "),
    ". Install them before running this script.",
    call. = FALSE
  )
}

library(ggplot2)
library(scales)

plot_dir <- "article"
dir.create(plot_dir, showWarnings = FALSE)

wharton_blue <- "#011F5B"
wharton_red <- "#990000"
wharton_light_blue <- "#82AFD3"
wharton_gray <- "#6C6F73"
grid_gray <- "#E6E8EB"
zero_line_gray <- "#9AA0A6"

article_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 18, color = wharton_blue),
      plot.caption = element_text(size = 9, color = wharton_gray, hjust = 0),
      axis.title = element_text(face = "bold", color = wharton_blue),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.text = element_text(color = "#2D2D2D"),
      panel.grid.major = element_line(color = grid_gray, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(color = "#2D2D2D"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

save_article_plot <- function(plot, filename, width = 8.8, height = 5.4) {
  ggsave(
    file.path(plot_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 320,
    bg = "white"
  )
}

complete <- read_complete_data()
complete_final <- add_payroll_features(complete)
models <- fit_article_models(complete_final)
n_club_seasons <- nrow(complete_final)

club_centered <- complete_final |>
  group_by(team) |>
  mutate(
    payroll_vs_club_avg = relative_annual_wages - mean(relative_annual_wages, na.rm = TRUE),
    points_vs_club_avg = points - mean(points, na.rm = TRUE)
  ) |>
  ungroup()

pooled_lm <- models$pooled
team_fe_lm <- models$club_fe
interaction_lm <- models$interaction

pooled_slope <- coef(pooled_lm)[["relative_annual_wages"]]
team_fe_summary <- summary(team_fe_lm)$coefficients
team_fe_slope <- team_fe_summary["relative_annual_wages", "Estimate"]
team_fe_p <- team_fe_summary["relative_annual_wages", "Pr(>|t|)"]

interaction_summary <- summary(interaction_lm)$coefficients
other_slope <- interaction_summary["relative_annual_wages", "Estimate"]
big_six_delta <- interaction_summary["relative_annual_wages:big_six", "Estimate"]
big_6_slope <- other_slope + big_six_delta

model_line <- data.frame(
  relative_annual_wages = seq(
    min(complete_final$relative_annual_wages, na.rm = TRUE),
    max(complete_final$relative_annual_wages, na.rm = TRUE),
    length.out = 200
  )
)
model_line$points <- predict(pooled_lm, newdata = model_line)

plot_overall <- ggplot(complete_final, aes(relative_annual_wages, points)) +
  geom_point(aes(color = big_six_group), size = 2.6, alpha = 0.78) +
  geom_line(data = model_line, aes(y = points), color = wharton_blue, linewidth = 1.2) +
  annotate(
    "label",
    x = 2.55,
    y = 34,
    label = paste0("+", round(pooled_slope, 1), " points per\nleague-median payroll"),
    color = wharton_blue,
    fill = "white",
    size = 4,
    fontface = "bold"
  ) +
  scale_color_manual(values = c("Other" = wharton_light_blue, "Big Six" = wharton_red)) +
  scale_x_continuous(labels = label_number(accuracy = 0.1), breaks = seq(0.5, 4, by = 0.5)) +
  scale_y_continuous(breaks = seq(20, 100, by = 20)) +
  coord_cartesian(ylim = c(15, 103)) +
  labs(
    title = "Points vs. Relative Wage Bill",
    x = "Relative wage bill (1.0 = league median)",
    y = "League points",
    caption = paste0(
      "Each dot is one club-season (n = ",
      n_club_seasons,
      "), 2013-14 through 2024-25. Source: processed club-season wage and points data."
    )
  ) +
  article_theme()

save_article_plot(plot_overall, "01_overall_relationship.png")

plot_team_effects <- ggplot(club_centered, aes(payroll_vs_club_avg, points_vs_club_avg)) +
  geom_hline(yintercept = 0, color = zero_line_gray, linewidth = 0.85) +
  geom_vline(xintercept = 0, color = zero_line_gray, linewidth = 0.85) +
  geom_point(aes(color = big_six_group), size = 2.4, alpha = 0.78) +
  geom_smooth(method = "lm", se = TRUE, color = wharton_blue, fill = wharton_light_blue, linewidth = 1.1) +
  annotate(
    "label",
    x = 0.50,
    y = -23,
    label = paste0("+", round(team_fe_slope, 1), " points, p = ", round(team_fe_p, 2)),
    color = wharton_blue,
    fill = "white",
    size = 4,
    fontface = "bold"
  ) +
  scale_color_manual(values = c("Other" = wharton_light_blue, "Big Six" = wharton_red)) +
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-40, 40, by = 20)) +
  labs(
    title = "Within-Club Payroll and Points Deviations",
    x = "Payroll deviation from club average",
    y = "Points deviation from club average",
    caption = paste0(
      "Each dot is one club-season within-club deviation (n = ",
      n_club_seasons,
      "). Zero lines mark a club's average payroll and points."
    )
  ) +
  article_theme()

save_article_plot(plot_team_effects, "02_team_effects.png")

interaction_centered <- complete_final |>
  group_by(team) |>
  mutate(
    points_centered = points - mean(points, na.rm = TRUE),
    payroll_centered = relative_annual_wages - mean(relative_annual_wages, na.rm = TRUE)
  ) |>
  ungroup()

line_ranges <- interaction_centered |>
  group_by(big_six_group) |>
  summarize(
    x_min = min(payroll_centered, na.rm = TRUE),
    x_max = max(payroll_centered, na.rm = TRUE),
    .groups = "drop"
  )

line_interaction <- do.call(
  rbind,
  lapply(seq_len(nrow(line_ranges)), function(i) {
    group_name <- as.character(line_ranges$big_six_group[i])
    slope <- if (group_name == "Big Six") big_6_slope else other_slope
    x <- seq(line_ranges$x_min[i], line_ranges$x_max[i], length.out = 100)
    data.frame(
      big_six_group = factor(group_name, levels = c("Other", "Big Six")),
      payroll_centered = x,
      points_centered = slope * x
    )
  })
)

plot_interaction <- ggplot(interaction_centered, aes(payroll_centered, points_centered)) +
  geom_hline(yintercept = 0, color = zero_line_gray, linewidth = 0.85) +
  geom_vline(xintercept = 0, color = zero_line_gray, linewidth = 0.85) +
  geom_point(aes(color = big_six_group), size = 2.4, alpha = 0.75) +
  geom_line(data = line_interaction, aes(color = big_six_group), linewidth = 1.35) +
  annotate(
    "label",
    x = 0.52,
    y = 24,
    label = paste0("Other clubs: +", round(other_slope, 1), " points"),
    color = wharton_light_blue,
    fill = "white",
    size = 3.8,
    fontface = "bold"
  ) +
  annotate(
    "label",
    x = 0.56,
    y = -18,
    label = paste0("Big Six: ", round(big_6_slope, 1), " points"),
    color = wharton_red,
    fill = "white",
    size = 3.8,
    fontface = "bold"
  ) +
  scale_color_manual(values = c("Other" = wharton_light_blue, "Big Six" = wharton_red)) +
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(-40, 40, by = 20)) +
  labs(
    title = "Within-Club Payroll and Points Deviations by Club Type",
    x = "Payroll deviation from club average",
    y = "Points deviation from club average",
    caption = paste0(
      "Each dot is one club-season within-club deviation (n = ",
      n_club_seasons,
      "). Lines show implied within-club slopes from the interaction model."
    )
  ) +
  article_theme()

save_article_plot(plot_interaction, "03_big_six_interaction.png")

message("Saved article plots to ", normalizePath(plot_dir))
