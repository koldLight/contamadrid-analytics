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

source("R/util.R")

##########################################################################
# Functions
##########################################################################

load_meteo_data <- function() {
  dt <- fread("dat/meteo_hist.tsv", sep = "\t")
  dt[, date := as.Date(date)]
  dt
}

download_meteo_data <- function() {
  from <- as.Date("2001-01-01")
  to <- Sys.Date()
  current <- from
  it <- 0
  
  repeat {
    print(paste("Downloading meteo data for month", current))
    days <- days_in_month(current)
    url <- paste0("http://www.ogimet.com/cgi-bin/gsynres?ord=REV",
                  "&ndays=", days, 
                  "&ano=",   year(current),
                  "&mes=",   month(current),
                  "&day=",   days,
                  "&hora=",  "00",
                  "&ind=",   "08221") # Madrid - Barajas
    
    # Download, parse data and transform it
    html.tables <- readHTMLTable(url, stringsAsFactors = FALSE, skip.rows = 1:2)
    data <- html.tables[[3]][,c(1,4,8,10)]
    colnames(data) <- c("date", "mean_temp", "wind_speed", "rain")
    
    data$date <- as.Date(paste0(data$date, "/", year(current)), "%d/%m/%Y")
    data$mean_temp  <- as.numeric(data$mean_temp)
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
  meteo.data <- fix_NAs_closest_value(meteo.data, "mean_temp")
  meteo.data <- fix_NAs_closest_value(meteo.data, "wind_speed")
  meteo.data <- fix_NAs_closest_value(meteo.data, "rain")
  
  # Write the results to file
  write.table(meteo.data, "dat/meteo_hist.tsv", sep = "\t", row.names = FALSE)
}
