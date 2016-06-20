##########################################################################
# Luz Frias, 2016-06-12
# Downloads and cleans Madrid air data (current and historical)
#
# Data format detailed in
# http://datos.madrid.es/FWProjects/egob/contenidos/datasets/ficheros/Interprete_ficheros_%20calidad_%20del_%20aire_global.pdf
##########################################################################

##########################################################################
# Libraries
##########################################################################

library(reshape2)
library(data.table)

##########################################################################
# Constants
##########################################################################

HISTORICAL_AIR_DATA_URL <- "http://datos.madrid.es/egob/catalogo/201200-__ID__-calidad-aire-horario.zip"
HISTORICAL_AIR_DATA_MAPPING <- "dat/historical_year_download_id.tsv"
HISTORICAL_AIR_DATA_DIR <- "dat/air_hist/"
HISTORICAL_AIR_DATA_ZIP_DIR <- paste0(HISTORICAL_AIR_DATA_DIR, "zip/")
HISTORICAL_AIR_DATA_RAW_DIR <- paste0(HISTORICAL_AIR_DATA_DIR, "raw/")
HISTORICAL_AIR_DATA_RES_DIR <- paste0(HISTORICAL_AIR_DATA_DIR, "res/")
HISTORICAL_AIR_DATA_FORMAT <- c(
  station     = 8,
  variable    = 2,
  technique   = 2,
  periodicity = 2,
  year        = 2,
  month       = 2,
  day         = 2,
  value       = rep(6, 24)
)

##########################################################################
# Functions
##########################################################################

load_historical_air_data <- function(station.ids = NA) {
  files <- list.files(HISTORICAL_AIR_DATA_RES_DIR)
  
  read.year <- function(file) {
    f <- fread(paste0(HISTORICAL_AIR_DATA_RES_DIR, file))
    if (!is.na(station.ids)) {
      f <- f[station %in% station.ids]
    }
    f
  }
  
  data <- rbindlist(lapply(files, read.year))
}

load_contamination_variables <- function() {
  data <- fread("dat/contamination_variables.tsv")
}

download_historical_air_data <- function() {
  mapping <- read.csv(HISTORICAL_AIR_DATA_MAPPING, sep="\t")
  years <- mapping$year
  
  for(year in years) {
    print(paste("Downloading and cleaning data from", year))
    download_hourly_air_data(year)
  }
}

download_hourly_air_data <- function(year) {
  # If it doesn't exist yet, create the file structure for historical data
  create_file_structure()
  
  # Get the download id
  mapping <- read.csv(HISTORICAL_AIR_DATA_MAPPING, sep="\t")
  download.id <- mapping[mapping$year == year,]$id
  if (length(download.id) == 0) {
    warning(paste("No data for year", year))
    return()
  }
  
  # Build the url and download the zipped data
  url <- gsub("__ID__", download.id, HISTORICAL_AIR_DATA_URL)
  zip.filename <- paste0(HISTORICAL_AIR_DATA_ZIP_DIR, year, ".zip")
  raw.dirname  <- paste0(HISTORICAL_AIR_DATA_RAW_DIR, year, "/")
  res.filename <- paste0(HISTORICAL_AIR_DATA_RES_DIR, year, ".tsv")
  download.file(url, zip.filename, quiet = TRUE)
  
  # Unzip it and read all the files in it
  unzip(zip.filename, exdir = raw.dirname)
  files <- list.files(raw.dirname, recursive = TRUE)
  
  data <- do.call("rbind", lapply(files, function(file)
    clean_historical_air_data(paste0(raw.dirname, file))))
  
  write.table(data, res.filename, sep = "\t", row.names = FALSE)
}

clean_historical_air_data <- function(input.file) {
  widths <- HISTORICAL_AIR_DATA_FORMAT
  data <- read.fwf(input.file, widths, stringsAsFactors = FALSE)
  colnames(data) <- names(widths)
  
  # Melt data in order to have one row per hour
  data.melted <- melt(data, id.vars = 1:7, variable.name = "hour")
  data.melted$hour <- as.numeric(gsub("value([0-9]+)$",
                                      "\\1",
                                      as.character(data.melted[, 8]))) - 1
  
  # Split the value in the measured value and if it's valid
  data.melted$valid <- substr(data.melted$value, 6, 6) == "V"
  data.melted$value <- as.numeric(substr(data.melted$value, 1, 5))
  
  # If the value isn't numeric, mark the record as invalid.
  # This was observed only once, in 2013-08 record 32173
  data.melted[is.na(data.melted$value), "valid"] <- FALSE
  
  return(data.melted)
}

create_file_structure <- function() {
  create_directory(HISTORICAL_AIR_DATA_DIR)
  create_directory(HISTORICAL_AIR_DATA_ZIP_DIR)
  create_directory(HISTORICAL_AIR_DATA_RAW_DIR)
  create_directory(HISTORICAL_AIR_DATA_RES_DIR)
}

create_directory <- function(directory) {
  dir.create(directory, showWarnings = FALSE)
}
