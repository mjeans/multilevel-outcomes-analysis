# Fit a prespecified sequence of multilevel models.

suppressPackageStartupMessages({
  library(broom.mixed)
  library(dplyr)
  library(lme4)
  library(readr)
  library(tibble)
})

input_path <- "data/synthetic_longitudinal_outcomes.csv"
if (!file.exists(input_path)) {
  stop("Run R/01_generate_nested_data.R before fitting models.")
}

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
outcomes <- read_csv(input_path, show_col_types = FALSE) |>
  mutate(
    organization_id = factor(organization_id),
    person_id = factor(person_id),
    baseline_centered = baseline_score - mean(baseline_score)
  )

null_model <- lmer(
  outcome ~ 1 +
    (1 | organization_id) +
    (1 | person_id),
  data = outcomes,
  REML = FALSE
)

conditional_model <- lmer(
  outcome ~ time + program + baseline_centered + ses_z +
    multilingual +
    (1 | organization_id) +
    (1 | person_id),
  data = outcomes,
  REML = FALSE
)

growth_model <- lmer(
  outcome ~ time * program + baseline_centered + ses_z +
    multilingual +
    (1 + time | organization_id) +
    (1 | person_id),
  data = outcomes,
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)

fixed_effects <- tidy(
  growth_model,
  effects = "fixed",
  conf.int = TRUE,
  conf.level = 0.95
) |>
  select(term, estimate, std.error, statistic, conf.low, conf.high) |>
  mutate(
    across(
      c(estimate, std.error, statistic, conf.low, conf.high),
      ~ round(.x, 3)
    )
  )

variance_raw <- as.data.frame(VarCorr(null_model))
total_variance <- sum(variance_raw$vcov)
variance_components <- variance_raw |>
  transmute(
    level = case_when(
      grp == "organization_id" ~ "Organization",
      grp == "person_id" ~ "Person",
      TRUE ~ "Observation"
    ),
    variance = vcov,
    share_of_total = vcov / total_variance
  ) |>
  mutate(across(c(variance, share_of_total), ~ round(.x, 3)))

comparison_raw <- anova(conditional_model, growth_model)
model_comparison <- tibble(
  model = rownames(comparison_raw),
  parameters = comparison_raw$npar,
  AIC = comparison_raw$AIC,
  BIC = comparison_raw$BIC,
  log_likelihood = comparison_raw$logLik,
  deviance = comparison_raw$deviance,
  chi_square = comparison_raw$Chisq,
  df = comparison_raw$Df,
  p_value = comparison_raw[["Pr(>Chisq)"]]
) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

write_csv(fixed_effects, "outputs/fixed_effects.csv")
write_csv(variance_components, "outputs/variance_components.csv")
write_csv(model_comparison, "outputs/model_comparison.csv")
saveRDS(growth_model, "outputs/growth_model.rds")

if (isSingular(growth_model, tol = 1e-4)) {
  warning("The random-effects structure is singular; simplify before inference.")
}

print(fixed_effects)
print(variance_components)
