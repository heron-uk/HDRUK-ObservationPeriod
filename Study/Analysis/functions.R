createLog <- function() {
  fileName <- here::here("Results", format(Sys.time(), "log_%Y_%m_%d_%H_%M_%S.txt"))
  options(omopgenerics_logger = fileName)
  logMessage("Logger created")
}
logMessage <- function(message) {
  date <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cli::cli_inform(paste0("{.pkg ", date, "} ", message))
  con <- file(getOption("omopgenerics_logger"), open = "a")
  writeLines(paste0(date, " - ", message), con)
  close(con)
  invisible(NULL)
}
generateMinDateObservationPeriod <- function(cdm, dataEndDate, censorAge) {
  # initial validation
  dataEndDate <- as.Date(dataEndDate)
  omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertDate(dataEndDate, length = 1)

  tables <- c(
    "drug_exposure", "condition_occurrence", "procedure_occurrence",
    "visit_occurrence", "device_exposure", "measurement", "observation", "death"
  )
  nm1 <- omopgenerics::uniqueTableName()
  nm2 <- omopgenerics::uniqueTableName()
  for (k in seq_along(tables)) {
    table <- tables[k]
    startDate <- omopgenerics::omopColumns(table = table, field = "start_date")
    xk <- cdm[[table]] |>
      dplyr::group_by(.data$person_id) |>
      dplyr::summarise(
        start_date = min(as.Date(.data[[startDate]]), na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::compute(name = nm1)
    if (k > 1) {
      x <- x |>
        dplyr::full_join(xk, by = "person_id") |>
        dplyr::mutate(min_date = dplyr::case_when(
          is.na(.data$min_date) ~ .data$start_date,
          is.na(.data$start_date) ~ .data$min_date,
          .data$min_date < .data$start_date ~ .data$min_date,
          .default = .data$start_date
        )) |>
        dplyr::select("person_id", "min_date") |>
        dplyr::compute(name = nm2)
    } else {
      x <- xk |>
        dplyr::rename("min_date" = "start_date") |>
        dplyr::compute(name = nm2)
    }
  }
  omopgenerics::dropSourceTable(cdm = cdm, name = nm1)

  cdm$observation_period <- x |>
    PatientProfiles::addDateOfBirth(name = nm2) |>
    dplyr::rename(observation_period_start_date = "min_date") |>
    dplyr::mutate(
      observation_period_end_date = as.Date(clock::add_years(.data$date_of_birth, !!as.integer(censorAge))),
    ) |>
    censorObservation(dataEndDate = dataEndDate) |>
    dplyr::mutate(
      period_type_concept_id = 0L,
      observation_period_id = dplyr::row_number()
    ) |>
    dplyr::select(
      "observation_period_id", "person_id", "observation_period_start_date",
      "observation_period_end_date", "period_type_concept_id"
    ) |>
    dplyr::compute(name = "observation_period")
  omopgenerics::dropSourceTable(cdm = cdm, name = nm2)

  return(cdm)
}
generateMinMaxObservationPeriod <- function(cdm, dataEndDate) {
  # initial validation
  omopgenerics::validateCdmArgument(cdm = cdm)

  tables <- c(
    "drug_exposure", "condition_occurrence", "procedure_occurrence",
    "visit_occurrence", "device_exposure", "measurement", "observation", "death"
  )
  nm1 <- omopgenerics::uniqueTableName()
  nm2 <- omopgenerics::uniqueTableName()
  for (k in seq_along(tables)) {
    table <- tables[k]
    startDate <- omopgenerics::omopColumns(table = table, field = "start_date")
    endDate <- omopgenerics::omopColumns(table = table, field = "end_date")
    xk <- cdm[[table]] |>
      dplyr::group_by(.data$person_id) |>
      dplyr::summarise(
        start_date = min(as.Date(.data[[startDate]]), na.rm = TRUE),
        end_date = max(as.Date(.data[[endDate]]), na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::compute(name = nm1)
    if (k > 1) {
      x <- x |>
        dplyr::full_join(xk, by = "person_id") |>
        dplyr::mutate(
          min_date = dplyr::case_when(
            is.na(.data$min_date) ~ .data$start_date,
            is.na(.data$start_date) ~ .data$min_date,
            .data$min_date < .data$start_date ~ .data$min_date,
            .default = .data$start_date
          ),
          max_date = dplyr::case_when(
            is.na(.data$max_date) ~ .data$end_date,
            is.na(.data$end_date) ~ .data$max_date,
            .data$max_date > .data$end_date ~ .data$max_date,
            .default = .data$end_date
          )
        ) |>
        dplyr::select("person_id", "min_date", "max_date") |>
        dplyr::compute(name = nm2)
    } else {
      x <- xk |>
        dplyr::rename("min_date" = "start_date", "max_date" = "end_date") |>
        dplyr::compute(name = nm2)
    }
  }
  omopgenerics::dropSourceTable(cdm = cdm, name = nm1)

  cdm$observation_period <- x |>
    dplyr::rename(
      observation_period_start_date = "min_date",
      observation_period_end_date = "max_date"
    ) |>
    censorObservation(dataEndDate = dataEndDate) |>
    dplyr::mutate(
      period_type_concept_id = 0L,
      observation_period_id = dplyr::row_number()
    ) |>
    dplyr::select(
      "observation_period_id", "person_id", "observation_period_start_date",
      "observation_period_end_date", "period_type_concept_id"
    ) |>
    dplyr::compute(name = "observation_period")
  omopgenerics::dropSourceTable(cdm = cdm, name = nm2)

  return(cdm)
}
generateVisitObservationPeriod <- function(cdm, name) {
  # initial validation
  omopgenerics::validateCdmArgument(cdm = cdm)

  tables <- c(
    "drug_exposure", "condition_occurrence", "procedure_occurrence",
    "visit_occurrence", "device_exposure", "measurement", "observation", "death"
  )
  for (k in seq_along(tables)) {
    table <- tables[k]
    startDate <- omopgenerics::omopColumns(table = table, field = "start_date")
    endDate <- omopgenerics::omopColumns(table = table, field = "end_date")
    xk <- cdm[[table]] |>
      dplyr::select(
        "person_id",
        "start_date" = dplyr::all_of(startDate),
        "end_date" = dplyr::all_of(endDate)
      ) |>
      dplyr::mutate(
        start_date = as.Date(.data$start_date),
        end_date = as.Date(.data$end_date)
      )
    if (k > 1) {
      x <- x |>
        dplyr::union_all(xk)
    } else {
      x <- xk
    }
  }
  logMessage("join overlap")
  x <- x |>
    dplyr::mutate(end_date = dplyr::case_when(
      is.na(.data$end_date) ~ .data$start_date,
      .data$end_date < .data$start_date ~ .data$start_date,
      .default = .data$end_date
    )) |>
    CohortConstructor:::joinOverlap(
      name = name,
      gap = 0L,
      startDate = "start_date",
      endDate = "end_date",
      by = "person_id"
    )

  logMessage("final summary")
  cdm[[name]] <- x |>
    dplyr::rename(
      observation_period_start_date = "start_date",
      observation_period_end_date = "end_date"
    ) |>
    dplyr::mutate(
      period_type_concept_id = 0L,
      observation_period_id = dplyr::row_number()
    ) |>
    dplyr::select(
      "observation_period_id", "person_id", "observation_period_start_date",
      "observation_period_end_date", "period_type_concept_id"
    ) |>
    dplyr::compute(name = name)

  return(cdm)
}
generateObservationPeriod <- function(cdm,
                                      oname,
                                      persistence,
                                      surveillance,
                                      dataEndDate) {
  x <- cdm[[oname]] |>
    dplyr::select(
      "person_id", "observation_period_start_date",
      "observation_period_end_date"
    )
  if (surveillance) {
    x <- x |>
      dplyr::mutate(observation_period_end_date = as.Date(clock::add_days(
        .data$observation_period_end_date, .env$persistence
      )))
    gap <- 0L
  } else {
    gap <- persistence
  }

  cdm$observation_period <- x |>
    CohortConstructor:::joinOverlap(
      name = "observation_period",
      gap = gap,
      startDate = "observation_period_start_date",
      endDate = "observation_period_end_date",
      by = "person_id"
    ) |>
    censorObservation(dataEndDate = dataEndDate) |>
    dplyr::mutate(
      period_type_concept_id = 0L,
      observation_period_id = dplyr::row_number()
    ) |>
    dplyr::select(
      "observation_period_id", "person_id", "observation_period_start_date",
      "observation_period_end_date", "period_type_concept_id"
    ) |>
    dplyr::compute(name = "observation_period")
  return(cdm)
}
summaryInObservation <- function(cdm, mode) {

  if (omopgenerics::isTableEmpty(cdm$observation_period)) {
    logMessage(paste("Empty observation period for:", mode, "definition"))
    return(omopgenerics::emptySummarisedResult())
  }

  ageGroup1 <- list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, Inf))
  ageGroup2 <- list(c(0, 150), c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150))

  logMessage("summarise in observation")
  res1 <- OmopSketch::summariseInObservation(
    cdm$observation_period,
    interval = "years",
    output = c("records", "person-days"),
    ageGroup = ageGroup1,
    sex = TRUE
  )

  logMessage("summarise observation metrics")
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
        c("median", "q25", "q75", "min", "max", "mean", "sd"),
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

  logMessage("generate denominator cohort")
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
    cohortDateRange = as.Date(c("2012-01-01", NA)),
    sex = c("Both", "Female", "Male"),
    ageGroup = ageGroup2,
    daysPriorObservation = 0L
  ) |>
    suppressMessages()

  logMessage("calculate incidence")
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

  logMessage("bind result")
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
  q <- "as.integer(local(CDMConnector::datediff('{col1}', '{col2}'))){plusOne}" |>
    glue::glue(col1 = col1, col2 = col2, plusOne = plusOne) |>
    as.character() |>
    rlang::parse_exprs() |>
    rlang::set_names(nm = colname)
  x %>%
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
