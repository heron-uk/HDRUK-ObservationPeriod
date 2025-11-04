
# source functions and create logger
source(here::here("Analysis", "functions.R"))
omopgenerics::createLogFile(logFile = here::here("Results", "log_{date}_{time}"))

dataEndDate <- as.Date(dataEndDate)
omopgenerics::assertDate(dataEndDate, length = 1)

# create cdm object
omopgenerics::logMessage("Create cdm object")
cdm <- CDMConnector::cdmFromCon(
  con = con, cdmSchema = cdmSchema, writeSchema = writeSchema,
  writePrefix = writePrefix, cdmName = dbName
)

# snapshot
omopgenerics::logMessage("Extract cdm snapshot")
snapshot <- OmopSketch::summariseOmopSnapshot(cdm)

# instantiate antibiotics cohorts
omopgenerics::logMessage("Instantiate antibiotics cohorts")
codelist <- CodelistGenerator::getDrugIngredientCodes(
  cdm = cdm,
  name = c("azithromycin", "ciprofloxacin", "teicoplanin"),
  nameStyle = "{concept_name}"
)
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
  cdm = cdm, conceptSet = codelist, name = "outcome"
) |>
  suppressMessages()

# original observation period
omopgenerics::logMessage("Characterise original observation period")
originalResult <- summaryInObservation(cdm, "original_data")

# first to extract observation period
omopgenerics::logMessage("Create first to extract observation period")
cdm <- OmopConstructor::buildObservationPeriod(
  cdm = cdm,
  collapseDays = Inf,
  persistenceDays = Inf,
  dateRange = c(NA, dataEndDate),
  censorAge = censorAge
)
omopgenerics::logMessage("Characterise first to extract observation period")
minExtractResult <- summaryInObservation(cdm, "min_extract")

# min-max observation period
omopgenerics::logMessage("Create min-max observation period")
cdm <- OmopConstructor::buildObservationPeriod(
  cdm = cdm,
  collapseDays = Inf,
  persistenceDays = 0,
  dateRange = c(NA, dataEndDate),
  censorAge = censorAge
)
omopgenerics::logMessage("Characterise min-max observation period")
minMaxResult <- summaryInObservation(cdm, "min_max")

# impatient observation period
omopgenerics::logMessage("Create impatient observation period")
cdm <- generateImpatientObservationPeriod(cdm)
omopgenerics::logMessage("Characterise impatient observation period")
impatientResult <- summaryInObservation(cdm, "impatient")

combinations <- dplyr::tribble(
  ~collapse, ~persistence,
  1L, 0L,
  365L, 0L,
  365L, 364L,
  730L, 0L,
  730L, 729L
)
resultCollapsePersistence <- combinations |>
  purrr::pmap(\(collapse, persistence) {
    name_id <- paste0("collapse_", collapse, "_persistence", persistence)
    omopgenerics::logMessage(paste("Create observation period:", collapse, "-", persistence))

    cdm <- OmopConstructor::buildObservationPeriod(
      cdm = cdm,
      collapseDays = Inf,
      persistenceDays = Inf,
      dateRange = c(NA, dataEndDate),
      censorAge = censorAge
    )

    omopgenerics::logMessage("Characterise observation period")
    summaryInObservation(cdm, name_id)
  }) |>
  omopgenerics::bind()

# export data
omopgenerics::exportSummarisedResult(
  snapshot,
  originalResult,
  minExtractResult,
  minMaxResult,
  impatientResult,
  resultCollapsePersistence,
  minCellCount = minCellCount,
  fileName = "observation_period_characterisation_{cdm_name}",
  path = here::here("Results")
)
