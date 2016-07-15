##########################################################################
# Luz Frias, 2016-06-19
# Downloads meteo data of Madrid
##########################################################################

##########################################################################
# Libraries
##########################################################################

library(XML)
library(lubridate)
library(data.table)
library(zoo)

source("R/util.R")

##########################################################################
# Constants
##########################################################################

METEO.DATE.FROM <- "2001-01-01"

##########################################################################
# Functions
##########################################################################

load_meteo_data <- function() {
  dt <- fread("dat/meteo_hist.tsv", sep = "\t")
  dt[, date := as.Date(date)]
  dt
}

download_meteo_data <- function() {
  hourly <- download_hourly_meteo_data()
  daily  <- download_daily_meteo_data()
  
  meteo.data <- merge(daily, hourly, by = "date")
  setkey(meteo.data, date, hour)
  
  write.table(meteo.data, file = "dat/meteo_hist.tsv", row.names = FALSE, sep = "\t")
}

download_daily_meteo_data <- function() {
  from <- as.Date(METEO.DATE.FROM)
  to <- Sys.Date()
  current <- from
  it <- 0
  
  repeat {
    print(paste("Downloading daily meteo data for month", current))
    days <- days_in_month(current)
    url <- paste0("http://www.ogimet.com/cgi-bin/gsynres?ord=REV",
                  "&ndays=", days, 
                  "&ano=",   year(current),
                  "&mes=",   month(current),
                  "&day=",   days,
                  "&hora=",  "00",
                  "&ind=",   "08221") # Madrid - Barajas
    
    # Download and parse data
    html.tables <- readHTMLTable(url, stringsAsFactors = FALSE, skip.rows = 1:2)
    meteo.table <- html.tables[[3]]
    
    # Rarely the html table has an extra column
    table.ncol <- ncol(meteo.table)
    if (table.ncol == 23) {
      col.ind <- c(1,2,3,4,6,8,11)
    } else if (table.ncol == 22) {
      col.ind <- c(1,2,3,4,6,8,10)
    } else {
      stop(paste("Unrecognized table format in ", url))
    }
    
    data <- meteo.table[, col.ind]
    colnames(data) <- c("date", "max_temp", "min_temp","mean_temp", "rel_humidity_pct",
                        "wind_speed", "rain")
    
    # Transform it
    data$date <- as.Date(paste0(data$date, "/", year(current)), "%d/%m/%Y")
    data$max_temp  <- as.numeric(data$max_temp)
    data$min_temp  <- as.numeric(data$min_temp)
    data$mean_temp  <- as.numeric(data$mean_temp)
    data$rel_humidity_pct  <- as.numeric(data$rel_humidity_pct)
    data$wind_speed <- as.numeric(data$wind_speed)
    data$rain <-       as.numeric(data$rain)
    
    # Append the data
    if (it == 0) {
      meteo.data <- data
    } else {
      meteo.data <- rbind(meteo.data, data)
    }
    it <- it + 1
    
    # Next iteration
    month(current) <- month(current) + 1
    
    if (current > to) {
      break
    }
  }
  
  # Transform to data.table and fix NA with closest row
  meteo.data <- as.data.table(meteo.data)
  setkey(meteo.data, date)
  meteo.data <- fix_NAs_closest_value(meteo.data, "max_temp")
  meteo.data <- fix_NAs_closest_value(meteo.data, "min_temp")
  meteo.data <- fix_NAs_closest_value(meteo.data, "mean_temp")
  meteo.data <- fix_NAs_closest_value(meteo.data, "rel_humidity_pct")
  meteo.data <- fix_NAs_closest_value(meteo.data, "wind_speed")
  meteo.data <- fix_NAs_closest_value(meteo.data, "rain")
  
  meteo.data
}


download_hourly_meteo_data <- function() {
  from <- as.Date(METEO.DATE.FROM)
  to <- Sys.Date()
  current <- from
  it <- 0
  
  repeat {
    print(paste("Downloading hourly meteo data for month", current))
    days <- days_in_month(current)
    url <- paste0("http://www.ogimet.com/cgi-bin/gsynres?ord=REV",
                  "&decoded=yes",     # hourly
                  "&ndays=", days, 
                  "&ano=",   year(current),
                  "&mes=",   month(current),
                  "&day=",   days,
                  "&hora=",  "24",
                  "&ind=",   "08221") # Madrid - Barajas
    
    # Download and parse data
    html.tables <- readHTMLTable(url, stringsAsFactors = FALSE, skip.rows = 1:2)
    meteo.table <- html.tables[[3]]
    
    # Rarely the html table has an extra column
    table.ncol <- ncol(meteo.table)
    
    data <- meteo.table[, c(1,2,3)]
    colnames(data) <- c("date", "hour", "temp")
    
    # Transform it
    data$date <- as.Date(paste0(data$date, "/", year(current)), "%d/%m/%Y")
    data$hour <- as.numeric(gsub("([0-9]+):[0-9]+", "\\1", data$hour))
    
    # Append the data
    if (it == 0) {
      meteo.data <- data
    } else {
      meteo.data <- rbind(meteo.data, data)
    }
    it <- it + 1
    
    # Next iteration
    month(current) <- month(current) + 1
    
    if (current > to) {
      break
    }
  }
  
  # Transform to data.table
  meteo.data <- as.data.table(meteo.data)
  setkey(meteo.data, date, hour)
  meteo.data <- unique(meteo.data)
  
  # Until 2007, there are values every 3 hours. From then, every hour
  # Fix the missing data by interpolation
  all.dates <- data.table(date = seq(min(meteo.data$date), max(meteo.data$date), by = 1))
  all.hours <- data.table(hour = 0:23)
  all <- all.dates[, as.list(all.hours), by = date]
  setkey(all, date, hour)
  
  full.data <- merge(meteo.data, all, by = c("date", "hour"), all.y = TRUE)
  full.data <- full.data[!(date == to & is.na(temp))]
  setkey(full.data, date, hour)
  full.data$temp <- na.approx(full.data$temp)
  
  full.data
}
