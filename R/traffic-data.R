##########################################################################
# Luz Frias, 2016-06-26
# Downloads traffic data from Madrid
##########################################################################

##########################################################################
# Libraries
##########################################################################

library(XML)
library(data.table)
library(lubridate)

source("R/util.R")

##########################################################################
# Functions
##########################################################################

load_traffic_data <- function() {
  dt <- fread("dat/traffic_hist.tsv", sep = "\t")
  dt[, date := as.Date(date)]
  setkey(dt, date)
  dt
}

download_traffic_data <- function() {
  # Download and parse traffic data
  raw <- xmlParse("http://www.mc30.es/images/xml/historicousuarios.xml")
  df <- xmlToDataFrame(raw)
  
  # Remove first row and column, created by first xml node indicating last update
  df <- df[-1, -1]
  
  # Parse needed values
  df$date <- as.Date(df$Fecha, format = "%d/%m/%Y")
  df$m30_density <- as.numeric(gsub("([0-9]+) veh x Km", "\\1", df$vehxKmTotales))
  
  # To data.table
  dt <- as.data.table(df[, c("date", "m30_density")])
  setkey(dt, date)
  
  # Fix NAs
  dt <- fix_traffic_NAs(dt)
  
  # Write the results
  write.table(dt, "dat/traffic_hist.tsv", sep = "\t", row.names = FALSE)
}

fix_traffic_NAs <- function(dt) {
  # Remove zero values and do an outer join with all the days between min and max dates
  dt <- dt[m30_density > 0]
  all.dates <- data.table(date = seq(min(dt$date), max(dt$date), "days"))
  dt <- merge(all.dates, dt, all.x = TRUE, by = "date")
  
  # Split in two data.tables by working or non-working day
  dt.w  <- dt[wday(date) %in% 2:6]
  dt.nw <- dt[wday(date) %in% c(1,7)]
  
  # Fix NAs by closest value
  dt.w  <- fix_NAs_closest_value(dt.w,  "m30_density")
  dt.nw <- fix_NAs_closest_value(dt.nw, "m30_density")
  
  # Join and return the result
  dt <- rbindlist(list(dt.w, dt.nw))
  setkey(dt, date)
  
  dt
}
