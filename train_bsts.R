##########################################################################
# Luz Frias, 2016-06-12
# Train the model
##########################################################################

library(data.table)
library(lubridate)
library(bsts)
library(ggplot2)

source("R/air-data.R")
source("R/meteo-data.R")
source("R/traffic-data.R")
source("R/calendar-data.R")

# Merge air, variable, meteo and traffic data
air.data <- load_historical_air_data()
var.data <- load_contamination_variables()
meteo.data <- load_meteo_data()
traffic.data <- load_traffic_data()
#calendar.data <- load_calendar_data()
air.data <- merge(air.data, var.data, by.x = "variable", by.y = "var_id")
air.data <- merge(air.data, meteo.data, by = c("date","hour"))
air.data <- merge(air.data, traffic.data, by = "date")
#air.data <- merge(air.data, calendar.data, by = "date")

setkey(air.data, var_formula, station, date, hour)

# Feature generation
air.data[,                      temp_range := max_temp - min_temp]
air.data[min_temp <= 5,         temp_range_inv := temp_range ]
air.data[is.na(temp_range_inv), temp_range_inv := 0]
air.data[,                      wday := as.factor(wday(date))]

# Feature normalization
air.data[, m30_density := m30_density / 1e6]

# Variable to predict normalization
air.data[, value_log := log(value)]

# Feature selection
prep.data <- air.data[valid == TRUE & var_formula == "NO2"]
prep.data[, formula_station := paste(var_formula, station, sep = "_")]
prep.data <- prep.data[, c("date", "formula_station", "hour", "temp_range_inv", "temp_range", "mean_temp",
                           "wind_speed", "rel_humidity_pct", "value", "value_log","wday", "m30_density"),
                       with = FALSE]

# Train a model for every station
for (form_station in unique(prep.data$formula_station)) {
  print(paste0("Training ", form_station))
  data <- prep.data[formula_station == form_station ]
  data[, formula_station := NULL]
  data[, week := floor_date(date, unit = "week")]
  
  # Train and test sets
  test.weeks <- c("2016-03-27", "2015-01-04", "2016-05-08", "2016-06-26", "2016-05-01", "2015-11-29",
                  "2015-12-20", "2015-12-13", "2015-11-01", "2016-06-12")

  for (pred.week.c in test.weeks) {
    pred.week <- as.Date(pred.week.c)
    it.train <- data[week <  pred.week]
    it.test  <- data[week == pred.week]
    
    # Model train, including hourly seasonality
    ss <- AddSeasonal(list(), y = it.train$value_log, nseasons = 24)
    bsts.model <- bsts(value_log ~ mean_temp + temp_range_inv + temp_range + wind_speed +
                         rel_humidity_pct + wday + m30_density,
                       data = it.train, state.specification = ss, niter = 100)
    
    pred.bsts.all    <- predict(bsts.model, newdata = it.test, quantiles = c(.05, .95))
    pred.bsts.mean   <- exp(pred.bsts.all$mean)
    pred.bsts.bottom <- pmax(0, exp(pred.bsts.all$interval[1,]))
    pred.bsts.top    <- exp(pred.bsts.all$interval[2,])
    
    # Print the errors
    print(paste0("BSTS MAE ",  form_station, ": ", mean(abs(pred.bsts.mean - it.test$value))))
    print(paste0("BSTS RMSE ", form_station, ": ", sqrt(mean(abs(pred.bsts.mean - it.test$value)^2))))
    
    dates <- it.test$date
    hour(dates) <- it.test$hour
    res <- data.table(date = dates, real = it.test$value, pred_mean = pred.bsts.mean,
                      bottom_interval = pred.bsts.bottom, top_interval = pred.bsts.top)
    hour(res$date) <- it.test$hour
    
    ggplot(res, aes(date)) +
      geom_line(aes(y = pred_mean, colour = "pred_mean")) +
      geom_line(aes(y = real, colour = "real")) +
      geom_line(aes(y = bottom_interval, colour = "bottom_interval")) +
      geom_line(aes(y = top_interval, colour = "top_interval"))
    
    ggsave(paste0("res/bsts_", form_station, "_", pred.week.c, ".png"),
           height = 20, width = 30,
           units = "cm", limitsize = FALSE)
  }
  
}
