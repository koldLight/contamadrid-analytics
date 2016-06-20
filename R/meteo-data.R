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
  to <- as.Date("2016-05-31")
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
    html.tables <- readHTMLTable(url, stringsAsFactors = FALSE, skip.rows = 1:2)
    data <- html.tables[[3]][,c(1,4,8,10)]
    colnames(data) <- c("date", "mean_temp", "wind_speed", "rain")
    
    data$date <- as.Date(paste0(data$date, "/", year(current)), "%d/%m/%Y")
    data$mean_temp  <- as.numeric(data$mean_temp)
    data$wind_speed <- as.numeric(data$wind_speed)
    data$rain <-       as.numeric(data$rain)
    if (length(which(is.na(data$mean_temp))) > 0)
      data[is.na(data$mean_temp), ]$mean_temp  <- mean(data$mean_temp,  na.rm = TRUE)
    if (length(which(is.na(data$wind_speed))) > 0)
      data[is.na(data$wind_speed),]$wind_speed <- mean(data$wind_speed, na.rm = TRUE)
    if (length(which(is.na(data$rain))) > 0)
      data[is.na(data$rain),      ]$rain       <- mean(data$rain,       na.rm = TRUE)
    
    if (it == 0) {
      meteo.data <- data
    } else {
      meteo.data <- rbind(meteo.data, data)
    }
    it <- it + 1
    
    month(current) <- month(current) + 1
    
    if (current > to) {
      break
    }
  }
  
  meteo.data <- meteo.data[order(meteo.data$date),]
  write.table(meteo.data, "dat/meteo_hist.tsv", sep = "\t", row.names = FALSE)
}