source("scripts/00_common.R")

coef_row <- function(model, term, model_name, coefficient_name, interpretation) {
  coef_table <- summary(model)$coefficients
  data.frame(
    model = model_name,
    coefficient = coefficient_name,
    term = term,
    estimate = unname(coef_table[term, "Estimate"]),
    std_error = unname(coef_table[term, "Std. Error"]),
    p_value = unname(coef_table[term, "Pr(>|t|)"]),
    interpretation = interpretation,
    stringsAsFactors = FALSE
  )
}

fit_row <- function(model, model_name) {
  model_summary <- summary(model)
  data.frame(
    model = model_name,
    adjusted_r_squared = unname(model_summary$adj.r.squared),
    residual_standard_error = unname(model_summary$sigma),
    n = length(model$residuals),
    stringsAsFactors = FALSE
  )
}

complete <- read_complete_data()
complete_final <- add_payroll_features(complete)
models <- fit_article_models(complete_final)

pooled <- coef_row(
  models$pooled,
  "relative_annual_wages",
  "Pooled linear",
  "Relative wage bill",
  "Strong league-wide association"
)

club_fe <- coef_row(
  models$club_fe,
  "relative_annual_wages",
  "Club fixed effects",
  "Relative wage bill",
  "Weak average within-club association"
)

interaction_other <- coef_row(
  models$interaction,
  "relative_annual_wages",
  "Interaction model",
  "Non-Big-Six wage slope",
  "Strong positive non-Big-Six association"
)

interaction_delta <- coef_row(
  models$interaction,
  "relative_annual_wages:big_six",
  "Interaction model",
  "Big Six interaction",
  "Big Six slope is much flatter"
)

interaction_big_six <- coef_row(
  models$interaction_big_six_base,
  "relative_annual_wages",
  "Interaction model",
  "Implied Big Six wage slope",
  "No clear Big Six wage return"
)

log_fe <- coef_row(
  models$log_fe,
  "log(relative_annual_wages)",
  "Log fixed effects",
  "Log relative wage bill",
  "Alternative diminishing-returns specification"
)

model_estimates <- bind_rows(
  pooled,
  club_fe,
  interaction_other,
  interaction_delta,
  interaction_big_six,
  log_fe
)

model_fit <- bind_rows(
  fit_row(models$pooled, "Pooled linear"),
  fit_row(models$club_fe, "Club fixed effects"),
  fit_row(models$interaction, "Interaction model"),
  fit_row(models$log_fe, "Log fixed effects")
)

dir.create("outputs", showWarnings = FALSE)
write.table(
  model_estimates,
  "outputs/model_estimates.csv",
  sep = ",",
  quote = FALSE,
  row.names = FALSE
)
write.table(
  model_fit,
  "outputs/model_fit.csv",
  sep = ",",
  quote = FALSE,
  row.names = FALSE
)

print(model_estimates)
print(model_fit)

message("Wrote outputs/model_estimates.csv and outputs/model_fit.csv")
