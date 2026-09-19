required_packages <- c("dplyr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running the pipeline.",
    call. = FALSE
  )
}

library(dplyr)

big_six_teams <- c(
  "Arsenal",
  "Chelsea",
  "Liverpool",
  "Manchester City",
  "Manchester United",
  "Tottenham Hotspur"
)

read_complete_data <- function(path = "data/processed/complete_14-25.csv.gz") {
  read.csv(path, stringsAsFactors = FALSE)
}

add_payroll_features <- function(data) {
  data |>
    group_by(season) |>
    mutate(
      season_median_wages_gbp = median(annual_wages_gbp, na.rm = TRUE),
      relative_annual_wages = annual_wages_gbp / season_median_wages_gbp
    ) |>
    ungroup() |>
    mutate(
      big_six = as.integer(team %in% big_six_teams),
      big_six_group = if_else(big_six == 1, "Big Six", "Other"),
      big_six_group = factor(big_six_group, levels = c("Other", "Big Six"))
    )
}

fit_article_models <- function(data) {
  list(
    pooled = lm(points ~ relative_annual_wages, data = data),
    club_fe = lm(points ~ relative_annual_wages + team, data = data),
    interaction = lm(points ~ relative_annual_wages + team + relative_annual_wages:big_six, data = data),
    interaction_big_six_base = lm(
      points ~ relative_annual_wages + team + relative_annual_wages:I(1 - big_six),
      data = data
    ),
    log_fe = lm(points ~ log(relative_annual_wages) + team, data = data)
  )
}
