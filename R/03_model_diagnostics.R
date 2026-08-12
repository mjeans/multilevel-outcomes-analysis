# Produce residual, trajectory, and organization-effect diagnostics.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(lme4)
  library(readr)
  library(tibble)
})

data_path <- "data/synthetic_longitudinal_outcomes.csv"
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
  geom_point(alpha = 0.12, color = "#166534") +
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

trajectory_summary <- outcomes |>
  group_by(program, time) |>
  summarise(
    mean_outcome = mean(outcome),
    standard_error = sd(outcome) / sqrt(n()),
    .groups = "drop"
  ) |>
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
      ymin = mean_outcome - 1.96 * standard_error,
      ymax = mean_outcome + 1.96 * standard_error
    ),
    width = 0.08
  ) +
  scale_color_manual(values = c("#64748B", "#15803D")) +
  scale_x_continuous(breaks = 0:2) +
  labs(
    title = "Observed outcome trajectories",
    x = "Measurement wave",
    y = "Mean outcome",
    color = NULL
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

organization_effects <- ranef(
  growth_model,
  condVar = TRUE
)$organization_id |>
  as.data.frame() |>
  rownames_to_column("organization_id") |>
  transmute(
    organization_id,
    random_intercept = .data[["(Intercept)"]]
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
  geom_point(color = "#166534", size = 1.5) +
  labs(
    title = "Organization random-intercept estimates",
    x = "Conditional deviation from the grand intercept",
    y = "Organization"
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
