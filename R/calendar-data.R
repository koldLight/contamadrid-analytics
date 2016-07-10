##########################################################################
# Luz Frias, 2016-07-10
# Loads bank holidays of Madrid
##########################################################################

##########################################################################
# Libraries
##########################################################################

library(lubridate)
library(data.table)

load_calendar_data <- function() {
  dt <- fread("dat/calendar.tsv", sep = "\t")
  dt[, date := as.Date(date)]
  dt
}

build_calendar_data <- function() {
  # Load the holidays
  holidays <- fread("dat/holidays.tsv", sep = "\t")
  holidays[, date := as.Date(date)]
  holidays[, is_holiday := TRUE]
  
  # Build a calendar with all the days
  from <- floor_date(min(holidays$date), "year")
  to <- ceiling_date(max(holidays$date), "year")
  all.dates <- data.table(date = seq(from, to, "days"))
  calendar <- merge(all.dates, holidays, all.x = TRUE, by = "date")
  
  # Mark saturday and sunday as holiday
  calendar[wday(date) %in% c(7,1), is_holiday := TRUE]
  
  # The rest of days are not a holiday
  calendar[is.na(is_holiday), is_holiday := FALSE]
  
  # Write the results to file
  write.table(calendar, "dat/calendar.tsv", sep = "\t", row.names = FALSE)
}
