##########################################################################
# Luz Frias, 2016-06-12
# Charts with the contamination evolution in Madrid
##########################################################################

library(data.table)
library(ggplot2)
library(lubridate)

source("R/air-data.R")

# pza españa: 28079004
air.data <- load_historical_air_data()
var.data <- load_contamination_variables()[]
air.data <- merge(air.data, var.data, by.x = "variable", by.y = "var_id")

# Historic evolution
historic <- air.data[, .(mean = mean(value)),
                         #max  = max(value),
                         #min  = min(value)),
                     by = .(year, month, var_formula)]
historic[, date := as.Date(paste0(2000 + year, '-', month, '-01'), '%Y-%m-%d')]
historic[, year  := NULL]
historic[, month := NULL]

historic <- melt(historic, id.vars = c("date", "var_formula"))

ggplot(historic, aes(date, value, colour = variable)) +
  geom_line() +
  facet_grid(var_formula ~ ., scales = "free_y")
  
# Monthly evolution (within a year)
monthly <- air.data[, .(mean = mean(value)),
                        #max  = max(value),
                        #min  = min(value)),
                        by = .(month, day, var_formula)]
monthly[, date := as.Date(paste0(month, '-', day), '%m-%d')]
monthly[, month  := NULL]
monthly[, day := NULL]

monthly <- melt(monthly, id.vars = c("date", "var_formula"))

ggplot(monthly, aes(date, value, colour = variable)) +
  geom_line() +
  facet_grid(var_formula ~ ., scales = "free_y")

# Daily evolution (within a week)
daily <- copy(air.data)
daily[, wday := wday(as.Date(paste0(2000 + year, '-', month, '-', day), '%Y-%m-%d'), label = TRUE)]
daily <- daily[, .(mean = mean(value)),
                   #max  = max(value),
                   #min  = min(value)),
               by = .(wday, var_formula)]

daily <- melt(daily, id.vars = c("wday", "var_formula"))

ggplot(daily, aes(wday, value, group = 1, colour = variable)) +
  geom_line() +
  facet_grid(var_formula ~ ., scales = "free_y")
