
library(here)
library(ggplot2)
library(dplyr)
library(visOmopResults)
library(tidyr)
library(ggh4x)

load(here("data", "shinyData.RData"))

# figure 1
x <- data$in_obs |>
  filter(
    .data$variable_name == "Number person-days",
    .data$age_group == "overall",
    .data$sex == "overall",
    .data$year >= 2012L & year <= 2024,
    !grepl("180|545|Bespoke", .data$mode)
  ) |>
  mutate(
    "OP definition" = case_when(
      mode == "First record to extract" ~ "First to extract",
      mode == "First to last record" ~ "First to last",
      mode == "Persistence 0" ~ "Ongoing",
      mode == "Persistence 365" ~ "Collapse 365",
      mode == "Persistence 365 + surveillance" ~ "Collapse + persistence 365",
      mode == "Persistence 730" ~ "Collapse 730",
      mode == "Persistence 730 + surveillance" ~ "Collapse + persistence 730",
      mode == "Inpatient hospitalisation" ~ "Inpatient"
    ),
    "10^5 person-years" = count / 365 / 100000,
  ) |>
  select("cdm_name", "10^5 person-years", "year", "OP definition")
x <- x |>
  union_all(
    x |>
      filter(cdm_name == "CPRD AURUM 50", `OP definition` == "First to extract") |>
      mutate("10^5 person-years" = 0L, "OP definition" = "Inpatient")
  )

ggplot(
  data = x, mapping = aes(x = year, y = `10^5 person-years`, colour = `OP definition`)
) +
  geom_line() +
  facet_grid(cols = vars(cdm_name), scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top")

in_obs <- data$in_obs

x <- data$obs_period |>
  addSettings() |>
  filter(
    !grepl("180|545|Bespoke", .data$mode) &
      variable_name %in% c("number records", "duration", "drugs_per_day")
  ) |>
  splitAll() |>
  pivotEstimates(pivotEstimatesBy = c("variable_name", "variable_level", "estimate_name")) |>
  mutate(
    "Days in observation" = `number records_count` * duration_mean,
    "Number observation periods" = `number records_count`,
    "Median duration" = duration_median,
    "Drug records per day" = drugs_per_day_mean,
    "OP definition" = case_when(
      mode == "First record to extract" ~ "First to extract",
      mode == "First to last record" ~ "First to last",
      mode == "Persistence 0" ~ "Ongoing",
      mode == "Persistence 365" ~ "Collapse 365",
      mode == "Persistence 365 + surveillance" ~ "Collapse + persistence 365",
      mode == "Persistence 730" ~ "Collapse 730",
      mode == "Persistence 730 + surveillance" ~ "Collapse + persistence 730",
      mode == "Inpatient hospitalisation" ~ "Inpatient"
    )
  ) |>
  select(
    "cdm_name", "OP definition", "Days in observation",
    "Number observation periods", "Median duration", "Drug records per day"
  )
reference <- x |>
  filter(`OP definition` == "First to extract") |>
  select(!"OP definition") |>
  rename(
    ref_days = "Days in observation",
    ref_obs = "Number observation periods",
    ref_dur = "Median duration",
    ref_drug = "Drug records per day"
  )
x <- x |>
  inner_join(reference, by = "cdm_name") |>
  mutate(
    `Days in observation` = `Days in observation` / ref_days,
    `Number observation periods` = `Number observation periods` / ref_obs,
    `Median duration` = `Median duration` / ref_dur,
    `Drug records per day` = `Drug records per day` / ref_drug
  ) |>
  filter(`OP definition` != "First to extract") |>
  select(!starts_with("ref")) |>
  mutate(
    `OP definition` = factor(x = `OP definition`, levels = c(
      "First to last", "Ongoing", "Collapse 365", "Collapse + persistence 365",
      "Collapse 730", "Collapse + persistence 730", "Inpatient"
    )),
    x = as.numeric(`OP definition`)
  ) |>
  pivot_longer(cols = c("Days in observation", "Number observation periods", "Median duration", "Drug records per day")) |>
  mutate(name = factor(name, levels = c(
    "Days in observation", "Number observation periods", "Median duration",
    "Drug records per day"
  )))

ggplot(
  data = x, mapping = aes(x = x, y = value, colour = `OP definition`, shape = cdm_name)
) +
  geom_hline(yintercept = 1) +
  geom_jitter(width = 0.2) +
  facet_wrap(facets = "name", scales = "free") +
  facetted_pos_scales(
    y = list(
      name == "Days in observation" ~ scale_y_log10(
        breaks = c(0.003, 0.01, 0.03, 0.1, 0.3, 1),
        labels = c(expression(3%*%10^-3), expression(10^-2), expression(3%*%10^-2), expression(10^-1), expression(3%*%10^-1), "1")
      ),
      name == "Number observation periods" ~ scale_y_log10(
        breaks = c(0.5, 1, 2, 4, 10, 20, 40)
      ),
      name == "Median duration" ~ scale_y_log10(
        breaks = c(0.001, 0.01, 0.1, 1),
        labels = c(expression(10^-3), expression(10^-2), expression(10^-1), "1")
      ),
      name == "Drug records per day" ~ scale_y_log10(
        breaks = c(1, 10, 100, 1000),
        labels = c("1", expression(10^1), expression(10^2), expression(10^3))
      )
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.box = "vertical",            # ⬅️ stack legends vertically
    legend.justification = "center",
    legend.box.just = "center",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  guides(
    colour = guide_legend(order = 1, title = "Observation period definition"),
    shape  = guide_legend(order = 2, title = "Database")
  )

# figure 2
x <- tibble(
  start = c(0),
  end = c(),
  type = c("visit_occurrence", "drug_exposure", "visit_occurrence", "drug_exposure")
)
verticalLines <- c(6, 300)

