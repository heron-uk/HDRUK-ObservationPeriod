summaryInObservation <- function(cdm, mode) {

  if (omopgenerics::isTableEmpty(cdm$observation_period)) {
    omopgenerics::logMessage(paste("Empty observation period for:", mode, "definition"))
    return(omopgenerics::emptySummarisedResult())
  }

  ageGroup1 <- list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, Inf))
  ageGroup2 <- list(c(0, 150), c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150))

  omopgenerics::logMessage("summarise in observation")
  res1 <- OmopSketch::summariseInObservation(
    cdm$observation_period,
    interval = "years",
    output = c("record", "person-days"),
    ageGroup = ageGroup1,
    sex = TRUE
  )

  omopgenerics::logMessage("summarise observation metrics")
  nm <- omopgenerics::uniqueTableName()
  characteristics <- cdm$observation_period |>
    dplyr::group_by(.data$person_id) |>
    dplyr::mutate(next_observation = dplyr::lead(
      .data$observation_period_start_date,
      order_by = .data$observation_period_start_date
    )) |>
    diffdate(
      col1 = c("observation_period_start_date", "observation_period_end_date"),
      col2 = c("observation_period_end_date", "next_observation"),
      colname = c("duration", "time_to_next_observation"),
      plusOne = c(TRUE, FALSE)
    ) |>
    dplyr::ungroup() |>
    PatientProfiles::addAgeQuery(
      indexDate = "observation_period_start_date",
      ageName = "age_start",
      ageGroup = list(age_group_start = ageGroup1)
    ) |>
    PatientProfiles::addAgeQuery(
      indexDate = "observation_period_end_date",
      ageName = "age_end",
      ageGroup = list(age_group_end = ageGroup1)
    ) |>
    PatientProfiles::addSexQuery() |>
    dplyr::select(!c("next_observation")) |>
    dplyr::compute(name = nm) |>
    dplyr::left_join(
      cdm$drug_exposure |>
        dplyr::select("person_id", start_date = "drug_exposure_start_date") |>
        dplyr::inner_join(cdm$observation_period, by = "person_id") |>
        dplyr::filter(
          .data$start_date >= .data$observation_period_start_date &
            .data$start_date <= .data$observation_period_end_date
        ) |>
        dplyr::group_by(.data$observation_period_id) |>
        dplyr::summarise(number_drugs = dplyr::n()),
      by = "observation_period_id"
    ) |>
    dplyr::compute(name = nm) |>
    dplyr::left_join(
      cdm$condition_occurrence |>
        dplyr::select("person_id", start_date = "condition_start_date") |>
        dplyr::inner_join(cdm$observation_period, by = "person_id") |>
        dplyr::filter(
          .data$start_date >= .data$observation_period_start_date &
            .data$start_date <= .data$observation_period_end_date
        ) |>
        dplyr::group_by(.data$observation_period_id) |>
        dplyr::summarise(number_conditions = dplyr::n()),
      by = "observation_period_id"
    ) |>
    dplyr::compute(name = nm) |>
    dplyr::left_join(
      cdm$visit_occurrence |>
        dplyr::select("person_id", start_date = "visit_start_date") |>
        dplyr::inner_join(cdm$observation_period, by = "person_id") |>
        dplyr::filter(
          .data$start_date >= .data$observation_period_start_date &
            .data$start_date <= .data$observation_period_end_date
        ) |>
        dplyr::group_by(.data$observation_period_id) |>
        dplyr::summarise(number_visits = dplyr::n()),
      by = "observation_period_id"
    ) |>
    dplyr::compute(name = nm) |>
    dplyr::collect() |>
    dplyr::mutate(
      drugs_per_day = .data$number_drugs / .data$duration,
      conditions_per_day = .data$number_conditions / .data$duration,
      visits_per_day = .data$number_visits / .data$duration
    ) |>
    PatientProfiles::summariseResult(
      variables = list(
        c("duration", "time_to_next_observation", "age_start", "age_end", "number_conditions", "number_drugs", "number_visits", "conditions_per_day", "drugs_per_day", "visits_per_day"),
        c("sex", "age_group_start", "age_group_end")
      ),
      estimates = list(
        c("median", "q25", "q75", "min", "max", "mean", "sd", "sum"),
        c("count", "percentage")
      ),
      counts = TRUE
    ) |>
    suppressMessages()
  recordsPerPerson <- cdm$observation_period |>
    dplyr::group_by(.data$person_id) |>
    dplyr::summarise(op_per_person = dplyr::n()) |>
    dplyr::inner_join(cdm$person |> dplyr::select("person_id"), by = "person_id") |>
    dplyr::collect() |>
    dplyr::mutate(op_per_person = dplyr::coalesce(as.integer(.data$op_per_person), 0L)) |>
    PatientProfiles::summariseResult(
      variables = "op_per_person",
      estimates = c("median", "q25", "q75", "min", "max", "mean", "sd"),
      counts = FALSE
    ) |>
    suppressMessages()
  omopgenerics::dropSourceTable(cdm = cdm, name = nm)

  omopgenerics::logMessage("generate denominator cohort")
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
    cohortDateRange = as.Date(c("2012-01-01", NA)),
    sex = c("Both", "Female", "Male"),
    ageGroup = ageGroup2,
    daysPriorObservation = 0L
  ) |>
    suppressMessages()

  omopgenerics::logMessage("calculate incidence")
  incidence <- IncidencePrevalence::estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years", "overall"),
    repeatedEvents = TRUE,
    outcomeWashout = 30,
    completeDatabaseIntervals = FALSE
  ) |>
    suppressMessages()
  omopgenerics::dropSourceTable(cdm = cdm, name = "denominator")

  omopgenerics::logMessage("bind result")
  result <- omopgenerics::bind(
    res1, incidence, characteristics, recordsPerPerson
  ) |>
    dplyr::mutate(cdm_name = omopgenerics::cdmName(cdm))
  result |>
    omopgenerics::newSummarisedResult(
      settings = omopgenerics::settings(result) |>
        dplyr::mutate(mode = .env$mode)
    )
}
diffdate <- function(x, col1, col2, colname, plusOne) {
  plusOne <- dplyr::if_else(plusOne, " + 1L", "")
  q <- "as.integer(clock::date_count_between(.data[['{col1}']], .data[['{col2}']], precision = 'day')){plusOne}" |>
    glue::glue(col1 = col1, col2 = col2, plusOne = plusOne) |>
    as.character() |>
    rlang::parse_exprs() |>
    rlang::set_names(nm = colname)
  x |>
    dplyr::mutate(!!!q)
}
censorObservation <- function(x, dataEndDate) {
  dataEndDate <- as.Date(dataEndDate)
  cdm <- omopgenerics::cdmReference(x)
  x |>
    dplyr::left_join(
      cdm$death |>
        dplyr::group_by(.data$person_id) |>
        dplyr::summarise(death_date = min(.data$death_date, na.rm = TRUE)),
      by = "person_id"
    ) |>
    dplyr::mutate(
      observation_period_end_date = dplyr::case_when(
        is.na(.data$death_date) ~ .data$observation_period_end_date,
        .data$death_date <= .data$observation_period_end_date ~ .data$death_date,
        .default = .data$observation_period_end_date
      ),
      observation_period_end_date = dplyr::if_else(
        .data$observation_period_end_date <= .env$dataEndDate,
        .data$observation_period_end_date,
        .env$dataEndDate
      )
    ) |>
    dplyr::filter(
      .data$observation_period_start_date <= .data$observation_period_end_date
    )
}
generateImpatientObservationPeriod <- function(cdm) {
  cdm$observation_period <- cdm$visit_occurrence |>
    dplyr::filter(.data$visit_concept_id %in% c(9201, 262)) |>
    dplyr::select("person_id", "visit_start_date", "visit_end_date") |>
    dplyr::mutate(
      visit_start_date = as.Date(.data$visit_start_date),
      visit_end_date = as.Date(.data$visit_end_date)
    ) |>
    CohortConstructor:::joinOverlap(
      name = "observation_period",
      gap = 0,
      startDate = "visit_start_date",
      endDate = "visit_end_date",
      by = "person_id"
    ) |>
    dplyr::mutate(
      period_type_concept_id = 0L,
      observation_period_id = dplyr::row_number()
    ) |>
    dplyr::select(
      "observation_period_id",
      "person_id",
      "observation_period_start_date" = "visit_start_date",
      "observation_period_end_date" = "visit_end_date",
      "period_type_concept_id"
    ) |>
    dplyr::compute(name = "observation_period")
  return(cdm)
}
