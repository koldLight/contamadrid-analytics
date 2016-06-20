##########################################################################
# Luz Frias, 2016-06-12
# Train the model
##########################################################################

library(data.table)
library(lubridate)
library(randomForest)

source("R/air-data.R")
source("R/meteo-data.R")

# pza españa: 28079004
# Merge air, variable and meteo data
air.data <- load_historical_air_data("28079004")
air.data[, date := as.Date(paste0(2000 + year, '-', month, '-01'), '%Y-%m-%d')]
var.data <- load_contamination_variables()[]
meteo.data <- load_meteo_data()
air.data <- merge(air.data, var.data, by.x = "variable", by.y = "var_id")
air.data <- merge(air.data, meteo.data, by = "date")

# Feature selection
data <- air.data[valid == TRUE & var_formula == "NO2"]
data[, wday := wday(date)]
data <- data[valid == TRUE,
             c("year", "month", "hour", "wday", "mean_temp", "wind_speed", "rain", "value"),
             with = FALSE]

# Train and test sets
set.seed(1234)
ind.test <- sample(1:nrow(data), nrow(data) * 0.2)
test <- data[ind.test]
train <- data[-ind.test]

# Training
rf <- randomForest(value ~ ., data = train, ntree = 100, do.trace = TRUE)
varImpPlot(rf)
plot(rf)

