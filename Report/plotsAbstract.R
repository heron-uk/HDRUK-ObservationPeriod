fileData <- file.path(getwd(), "data", "shinyData.RData")
if (!file.exists(fileData)) {
  # preprocess data if it has not been done
  source(file.path(getwd(), "data", "preprocess.R"))
} else {
  load(fileData)
}

characteristics <- data$obs_period |>
  dplyr::mutate(
    estimate_value = dplyr::if_else(
      stringr::str_detect(.data$variable_name, "_per_day"),
      as.character(1000 * as.numeric(.data$estimate_value)),
      .data$estimate_value
    )
  ) |>
  visOmopResults::formatEstimateValue(decimals = c(numeric = 0, integer = 0, percentage = 1)) |>
  visOmopResults::formatEstimateName(estimateName = c(
    "N (%)" = "<count> (<percentage>%)",
    "N" = "<count>",
    "median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
    "Range" = "<min> to <max>"
  )) |>
  omopgenerics::splitAll() |>
  omopgenerics::addSettings(settingsColumn = "mode") |>
  dplyr::mutate(
    variable_name = dplyr::if_else(
      .data$variable_name == "sex" & .data$variable_level == "Female",
      "Female", .data$variable_name
    )
  ) |>
  dplyr::filter(
    !.data$estimate_name %in% c("mean", "sd", "density_x", "density_y"),
    .data$estimate_name != "Range"
  ) |>
  dplyr::select(!c("result_id", "variable_level")) |>
  dplyr::inner_join(
    dplyr::tribble(
      ~variable_name, ~new_variable_name,
      "number records", "Number records",
      "number subjects", "Number subjects",
      "duration", "OP length",
      "time_to_next_observation", "Time between OP",
      "op_per_person", "OP per person",
      #"age_start", "Age (start)",
      #"age_end", "Age (end)",
      #"Female", "Female",
      #"drugs_per_day", "Drugs per 1,000 days",
      #"conditions_per_day", "Conditions per 1,000 days",
      #"visits_per_day", "Visits per 1,000 days"
    ) |>
      dplyr::mutate(order_id = dplyr::row_number()),
    by = "variable_name"
  ) |>
  dplyr::mutate(new_variable_name = dplyr::coalesce(.data$new_variable_name, .data$variable_name)) |>
  dplyr::arrange(.data$order_id) |>
  dplyr::select(!c("variable_name", "order_id")) |>
  dplyr::rename(variable_name = "new_variable_name") |>
  dplyr::relocate("variable_name") |>
  dplyr::filter(!grepl("180|545", .data$mode))

gt <- visOmopResults::visTable(
  result = characteristics,
  header = "cdm_name",
  groupColumn = "mode",
  hide = c("estimate_name", "estimate_type")
)

gt::gtsave(gt, here::here("figures", "obs.docx"))

x <- data$in_obs |>
  dplyr::filter(
    .data$variable_name == "Number person-days",
    .data$age_group == "overall",
    .data$sex == "overall",
    .data$year >= 2012L,
    !grepl("180|545", .data$mode)
  )
x <- x |>
  dplyr::union_all(
    x |>
      dplyr::filter(cdm_name == "CPRD AURUM 50", mode == "Bespoke") |>
      dplyr::mutate(count = 0L, mode = "Inpatient hospitalisation")
  ) |>
  dplyr::mutate(
    `person-years` = .data$count / 365,
    `OP definition` = .data$mode
  ) |>
  dplyr::arrange(`OP definition`)

p0 <- ggplot2::ggplot(
  data = x, mapping = ggplot2::aes(x = year, y = `person-years`, colour = `OP definition`)
) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap("cdm_name")

plotx <- function(x) {
  ggplot2::ggplot(
    data = x, mapping = ggplot2::aes(x = year, y = `person-years`, colour = `OP definition`)
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap("cdm_name") +
    ggplot2::theme(legend.position = "top")
}

p1 <- x |>
  dplyr::filter(cdm_name == "Barts Health") |>
  plotx() +
  ggplot2::xlim(c(2012, 2024)) +
  ggplot2::ylim(c(0, 3e6))

p2 <- x |>
  dplyr::filter(cdm_name == "IDRIL") |>
  plotx() +
  ggplot2::xlim(c(2012, 2024)) +
  ggplot2::ylim(c(0, 1.5e6))

p3 <- x |>
  dplyr::filter(cdm_name == "UCLH") |>
  plotx() +
  ggplot2::xlim(c(2019, 2024)) +
  ggplot2::ylim(c(0, 1.2e6))

p4 <- x |>
  dplyr::filter(cdm_name == "GOSH") |>
  plotx() +
  ggplot2::xlim(c(2018, 2024)) +
  ggplot2::ylim(c(0, 0.21e6))

p5 <- x |>
  dplyr::filter(cdm_name == "CPRD AURUM 50") |>
  plotx() +
  ggplot2::xlim(c(2012, 2024)) +
  ggplot2::ylim(c(0, 1.75e6))


library(patchwork)

p <- p1 + p2 + p3 + p4 + p5 +
  plot_layout(guides = "collect") &  # Collect legends
  theme(legend.position = "top")

x <- data$incidence |>
  dplyr::filter(
    .data$outcome == "ciprofloxacin",
    cdm_name != "CPRD AURUM 50",
    year == 2023, sex == "overall", age_group == "overall",
    grepl("incidence", .data$estimate_name)
  ) |>
  omopgenerics::pivotEstimates() |>
  dplyr::rename(`Observation period definition` = "mode", `Incidence per 100,000 py` = "incidence_100000_pys")

ggplot2::ggplot(
  data = x,
  mapping = ggplot2::aes(
    x = `Observation period definition`,
    y = `Incidence per 100,000 py`,
    ymax = incidence_100000_pys_95CI_upper,
    ymin = incidence_100000_pys_95CI_lower,
    colour = `Observation period definition`
  )
) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar() +
  ggplot2::facet_wrap("cdm_name", scales = "free_y") +
  ggplot2::scale_y_log10() +
  ggplot2::theme(
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank()
  )
