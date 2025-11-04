
# restore the renv environment
renv::activate()
renv::restore()

# create connection
con <- DBI::dbConnect("...")
library(odbc)
# database acronym (e.g. "CPRD AURUM")
dbName <- "..."

# schemas and prefix
cdmSchema <- "..."
writeSchema <- "..."
writePrefix <- "..."

# suppression
minCellCount <- 5L

# data cut
dataEndDate <- "..."
censorAge <- 150L

source(here::here("RunAnalysis.R"))
