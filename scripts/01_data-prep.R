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

wages <- read.csv("data/raw/wages_14-25.csv.gz", stringsAsFactors = FALSE)
points <- read.csv("data/raw/points_14-25.csv.gz", stringsAsFactors = FALSE)

complete <- points |>
  inner_join(wages, by = c("season", "team")) |>
  select(season, team, points, annual_wages_gbp)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write.table(
  complete,
  "data/processed/complete_14-25.csv.gz",
  sep = ",",
  quote = FALSE,
  row.names = FALSE
)

message("Wrote data/processed/complete_14-25.csv.gz (", nrow(complete), " rows)")
