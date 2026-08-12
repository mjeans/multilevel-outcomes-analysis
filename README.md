# Multilevel outcomes analysis

A longitudinal, three-level analysis of synthetic outcome data: repeated observations are nested within people, and people are nested within organizations. The project focuses on variance decomposition, change over time, contextual effects, and the interpretation of a time-by-program interaction.

The dataset is entirely synthetic and does not describe a real school, learner, client, or intervention.

## Analytic question

Do outcomes change differently over time in program organizations than in comparison organizations after accounting for baseline outcome level and individual characteristics?

## Why multilevel modeling

Ordinary regression would treat repeated observations and people from the same organization as independent. This workflow explicitly models:

- organization-level variation in baseline outcomes
- person-level variation within organizations
- repeated observations within people
- organization-level variation in growth over time
- the program-by-time interaction

## Model sequence

1. An unconditional random-intercept model partitions variance across organizations, people, and observations.
2. A conditional model adds time, program status, baseline score, socioeconomic status, and multilingual status.
3. The final model adds the time-by-program interaction and a random time slope at the organization level.
4. Residual, fitted-value, and random-effect diagnostics are produced before interpretation.

## Repository structure

~~~text
R/
  01_generate_nested_data.R
  02_fit_multilevel_models.R
  03_model_diagnostics.R
analysis/
  multilevel_outcomes.qmd
stata/
  multilevel_analysis.do
data/
  README.md
.github/workflows/
  validate-analysis.yml
~~~

## Run the analysis

With R 4.4 or later:

~~~r
install.packages(c(
  "broom.mixed", "dplyr", "ggplot2",
  "lme4", "purrr", "readr", "tibble", "tidyr"
))

source("R/01_generate_nested_data.R")
source("R/02_fit_multilevel_models.R")
source("R/03_model_diagnostics.R")
~~~

A companion Stata do-file fits the equivalent mixed-effects specification and produces marginal predictions. GitHub Actions validates the R implementation because Stata requires a commercial license.

## Outputs

- `outputs/fixed_effects.csv`
- `outputs/variance_components.csv`
- `outputs/model_comparison.csv`
- `outputs/residual_diagnostics.png`
- `outputs/program_trajectories.png`
- `outputs/organization_effects.png`

## Interpretation boundaries

The simulated program is assigned at the organization level, but the repository is a modeling demonstration—not evidence that the model alone establishes causality. A real cluster-level evaluation would address assignment or selection, baseline equivalence, implementation variation, missing waves, informative attrition, and the number of independent clusters.

## Skills demonstrated

Longitudinal analysis · hierarchical linear modeling · mixed-effects models · variance decomposition · interaction interpretation · model diagnostics · R · Stata

## License

MIT
