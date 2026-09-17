suppressPackageStartupMessages(library(lme4))
model <- readRDS("outputs/growth_model.rds")
trajectory <- read.csv("outputs/adjusted_trajectories.csv")
organization <- read.csv("outputs/organization_effects.csv")
variance <- read.csv("outputs/variance_components.csv")
stopifnot(!isSingular(model), is.null(model@optinfo$conv$lme4$messages),
  abs(fixef(model)["time:program"] - 1.55) < .5,
  nrow(trajectory) == 6, all(is.finite(trajectory$standard_error)),
  all(trajectory$standard_error > 0),
  all(trajectory$lower_95 < trajectory$mean_outcome),
  all(trajectory$upper_95 > trajectory$mean_outcome),
  nrow(organization) == 80, all(organization$conditional_se > 0),
  all(organization$lower_95 < organization$random_intercept),
  abs(sum(variance$share_of_total) - 1) < .005)
cat("Multilevel convergence, uncertainty, and synthetic recovery checks passed.\n")
