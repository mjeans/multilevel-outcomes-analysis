# Produce residual, trajectory, and organization-effect diagnostics.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(lme4)
  library(readr)
  library(tibble)
})

data_path <- "data/synthetic_longitudinal_outcomes.csv"
source("R/portfolio_report.R")
model_path <- "outputs/growth_model.rds"
if (!file.exists(data_path) || !file.exists(model_path)) {
  stop("Run the data-generation and model-fitting scripts first.")
}

outcomes <- read_csv(data_path, show_col_types = FALSE)
growth_model <- readRDS(model_path)

diagnostic_data <- tibble(
  fitted = fitted(growth_model),
  residual = resid(growth_model)
)

residual_plot <- ggplot(
  diagnostic_data,
  aes(x = fitted, y = residual)
) +
  geom_hline(yintercept = 0, color = "#475569", linewidth = 0.5) +
  geom_bin_2d(bins = 40) +
  scale_fill_gradient(low = "#e3efef", high = "#087e83", name = "Records") +
  geom_smooth(method = "loess", se = FALSE, color = "#B45309") +
  labs(
    title = "Conditional residuals versus fitted values",
    x = "Fitted outcome",
    y = "Residual"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "outputs/residual_diagnostics.png",
  residual_plot,
  width = 8,
  height = 5,
  dpi = 160
)

trajectory_summary <- expand.grid(program = 0:1, time = 0:2)
trajectory_summary$baseline_centered <- 0
trajectory_summary$ses_z <- mean(outcomes$ses_z)
trajectory_summary$multilingual <- mean(outcomes$multilingual)
fixed_design <- model.matrix(~ time * program + baseline_centered + ses_z + multilingual,
                             trajectory_summary)
fixed_design <- fixed_design[, names(fixef(growth_model)), drop = FALSE]
trajectory_summary$mean_outcome <- as.vector(fixed_design %*% fixef(growth_model))
trajectory_summary$standard_error <- sqrt(diag(fixed_design %*% vcov(growth_model) %*% t(fixed_design)))
trajectory_summary$lower_95 <- trajectory_summary$mean_outcome - qnorm(.975)*trajectory_summary$standard_error
trajectory_summary$upper_95 <- trajectory_summary$mean_outcome + qnorm(.975)*trajectory_summary$standard_error
trajectory_summary <- trajectory_summary |>
  mutate(
    program = factor(
      program,
      levels = c(0, 1),
      labels = c("Comparison", "Program")
    )
  )

trajectory_plot <- ggplot(
  trajectory_summary,
  aes(
    x = time,
    y = mean_outcome,
    color = program,
    group = program
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(
      ymin = lower_95,
      ymax = upper_95
    ),
    width = 0.08
  ) +
  scale_color_manual(values = c("#64748B", "#15803D")) +
  scale_x_continuous(breaks = 0:2) +
  labs(
    title = "Adjusted outcome trajectories",
    subtitle = "Synthetic data; model-based pointwise 95% Wald intervals",
    x = "Measurement wave",
    y = "Mean outcome",
    color = NULL,
    caption = "Pooled covariate distribution; mixed-model covariance accounts for nesting. Not prediction intervals."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

ggsave(
  "outputs/program_trajectories.png",
  trajectory_plot,
  width = 8,
  height = 5,
  dpi = 160
)

conditional_effects <- ranef(growth_model, condVar = TRUE)$organization_id
conditional_se <- sqrt(attr(conditional_effects, "postVar")[1, 1, ])
organization_effects <- conditional_effects |>
  as.data.frame() |>
  rownames_to_column("organization_id") |>
  transmute(
    organization_id,
    random_intercept = .data[["(Intercept)"]],
    conditional_se = conditional_se,
    lower_95 = random_intercept - qnorm(.975)*conditional_se,
    upper_95 = random_intercept + qnorm(.975)*conditional_se
  ) |>
  arrange(random_intercept) |>
  mutate(
    organization_id = factor(
      organization_id,
      levels = organization_id
    )
  )

organization_plot <- ggplot(
  organization_effects,
  aes(x = random_intercept, y = organization_id)
) +
  geom_vline(xintercept = 0, color = "#94A3B8", linewidth = 0.5) +
  geom_segment(aes(x = lower_95, xend = upper_95, yend = organization_id),
               color = "#087e83", alpha = .65) +
  geom_point(color = "#166534", size = 1.5) +
  labs(
    title = "Organization deviations with conditional uncertainty",
    subtitle = "Synthetic organizations; approximate 95% conditional intervals",
    x = "Conditional deviation from the grand intercept",
    y = "Organization (ordered for display)",
    caption = "Conditional on fitted variance parameters. Overlapping estimates are not a reliable league table."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave(
  "outputs/organization_effects.png",
  organization_plot,
  width = 8,
  height = 7,
  dpi = 160
)

write_csv(trajectory_summary, "outputs/adjusted_trajectories.csv")
write_csv(organization_effects, "outputs/organization_effects.csv")
publish_plot(trajectory_plot, "assets/adjusted-trajectories.svg", "Adjusted outcome trajectories",
             "Mixed-model adjusted means by program and wave with pointwise 95% Wald intervals, accounting for organization and person nesting.")
publish_plot(organization_plot, "assets/organization-effects.svg", "Organization effects with conditional intervals",
             "Eighty synthetic organization random intercepts with approximate 95% conditional intervals; ordering does not establish true ranks.", height = 8)
publish_plot(residual_plot, "assets/residual-diagnostics.svg", "Residual diagnostics",
             "Conditional residuals against fitted values with a loess trend; synthetic model diagnostics.")
effects <- read_csv("outputs/fixed_effects.csv", show_col_types = FALSE)
variances <- read_csv("outputs/variance_components.csv", show_col_types = FALSE)
write_research_report(c("# Executed multilevel outcomes report", "",
  "## Question and design", "",
  "How does the program-associated trajectory differ across three waves when people are nested in organizations? The synthetic model contains organization random intercepts/slopes and person random intercepts.", "",
  "## Adjusted trajectories", "", "![Adjusted trajectories](../assets/adjusted-trajectories.svg)", "",
  "Means average the linear fixed-effects model over pooled covariates. Intervals use the mixed-model fixed-effect covariance; they are pointwise Wald intervals, not individual prediction intervals or simultaneous bands.", "",
  "## Fixed effects", "", md_table(effects), "", "## Variance decomposition (null model)", "", md_table(variances), "",
  "## Conditional organization effects", "", "![Organization effects](../assets/organization-effects.svg)", "",
  "## Diagnostics and limits", "", "![Residual diagnostic](../assets/residual-diagnostics.svg)", "",
  "The model is checked for convergence, singularity, finite uncertainty, and recovery of the known 1.55-point program-by-wave interaction. The stored conditional-versus-growth comparison changes both fixed and random structure: its likelihood-ratio p-value is exploratory, not an isolated test of the interaction or a boundary-corrected random-slope test.", "",
  "These synthetic results are not a causal program evaluation. Conditional random-effect intervals hold estimated variance parameters fixed and should not be read as definitive rankings."))
