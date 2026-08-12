# Generate three-wave outcome data with people nested in organizations.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(tidyr)
})

set.seed(20260812)
dir.create("data", showWarnings = FALSE, recursive = TRUE)

n_organizations <- 80L

organizations <- tibble(
  organization_id = sprintf("O%03d", seq_len(n_organizations)),
  program = rbinom(n_organizations, 1, 0.48),
  n_people = sample(35:65, n_organizations, replace = TRUE),
  organization_intercept = rnorm(n_organizations, 0, 3.2),
  organization_time_slope = rnorm(n_organizations, 0, 0.65)
)

people <- tibble(
  organization_id = rep(
    organizations$organization_id,
    times = organizations$n_people
  )
) |>
  mutate(
    person_id = sprintf("P%05d", row_number()),
    baseline_score = pmin(pmax(rnorm(n(), 50, 9), 20), 80),
    ses_z = rnorm(n(), 0, 1),
    multilingual = rbinom(n(), 1, 0.22),
    person_intercept = rnorm(n(), 0, 4.0)
  ) |>
  left_join(
    organizations |>
      select(
        organization_id,
        program,
        organization_intercept,
        organization_time_slope
      ),
    by = "organization_id"
  )

longitudinal_data <- people |>
  crossing(time = 0:2) |>
  mutate(
    outcome =
      22 +
      0.58 * baseline_score +
      1.60 * ses_z -
      1.10 * multilingual +
      1.85 * time +
      0.80 * program +
      1.55 * time * program +
      organization_intercept +
      organization_time_slope * time +
      person_intercept +
      rnorm(n(), 0, 4.8)
  ) |>
  transmute(
    organization_id,
    person_id,
    program,
    time,
    baseline_score = round(baseline_score, 2),
    ses_z = round(ses_z, 3),
    multilingual,
    outcome = round(outcome, 2)
  ) |>
  arrange(organization_id, person_id, time)

stopifnot(
  n_distinct(longitudinal_data$organization_id) == n_organizations,
  all(count(longitudinal_data, person_id)$n == 3),
  all(longitudinal_data$time %in% 0:2),
  all(is.finite(longitudinal_data$outcome))
)

write_csv(
  longitudinal_data,
  "data/synthetic_longitudinal_outcomes.csv"
)
message(
  "Wrote ",
  nrow(longitudinal_data),
  " observations for ",
  n_distinct(longitudinal_data$person_id),
  " people."
)
