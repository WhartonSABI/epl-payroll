# install.packages("tidyverse")
library(tidyverse)

# read raw data
wages <- read_csv("data/raw/wages_14-25.csv", show_col_types = FALSE)
points <- read_csv("data/raw/points_14-25.csv", show_col_types = FALSE)

# merge wages + points on (season, team)
complete <- points |>
  inner_join(wages, by = c("season", "team")) |>
  select(-rank, -n_players, -weekly_wages_gbp, -pct_estimated)

# write output
write_csv(complete, "data/processed/complete_14-25.csv")
message("Wrote: data/processed/complete_14-25.csv (", nrow(complete), " rows)")
