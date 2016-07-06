##########################################################################
# Luz Frias, 2016-06-12
# Train the model
##########################################################################

library(data.table)
library(lubridate)
library(randomForest)
library(xgboost)
library(ggplot2)

source("R/air-data.R")
source("R/meteo-data.R")
source("R/traffic-data.R")

# pza españa: 28079004
# Merge air, variable, meteo and traffic data
air.data <- load_historical_air_data("28079004")
var.data <- load_contamination_variables()
meteo.data <- load_meteo_data()
traffic.data <- load_traffic_data()
air.data <- merge(air.data, var.data, by.x = "variable", by.y = "var_id")
air.data <- merge(air.data, meteo.data, by = "date")
air.data <- merge(air.data, traffic.data, by = "date")

# Feature generation
setkey(air.data, var_formula, station, date, hour)
# empeoran!
#air.data[, prev_1d_value  := shift(value, 24 * 1,  type = "lag"), by = .(var_formula, station)]
#air.data[, prev_3d_value  := shift(value, 24 * 3,  type = "lag"), by = .(var_formula, station)]
#air.data[, prev_5d_value  := shift(value, 24 * 5,  type = "lag"), by = .(var_formula, station)]
#air.data[, prev_10d_value := shift(value, 24 * 10, type = "lag"), by = .(var_formula, station)]

air.data[, wday := wday(date)]

# Feature selection
air.data <- air.data[valid == TRUE & var_formula == "NO2" & station == "28079004"]
air.data[, formula_station := paste(var_formula, station, sep = "_")]
air.data <- air.data[, c("formula_station", "year", "month", "hour", "wday", "mean_temp", "wind_speed",
                       "rain", "rel_humidity_pct", "value", "m30_density"),
                     with = FALSE]

# Train a model for every station and variable
for (form_station in unique(air.data$formula_station)) {
  print(paste0("Training ", form_station))
  data <- air.data[formula_station == form_station]
  data[, formula_station := NULL]
  
  # Train and test sets
  set.seed(1234)
  ind.test <- sample(1:nrow(data), nrow(data) * 0.2)
  test <- data[ind.test]
  train <- data[-ind.test]
  
  # Training with xgboost
  train.label <- train$value
  train.data <- train[, -c("value"), with = FALSE]
  test.label <- test$value
  test.data <- test[, -c("value"), with = FALSE]
  xgb <- xgboost(data = data.matrix(train.data), label = train.label, max.depth = 11,
                 nrounds = 400, objetive = "reg:linear", eval_metric = "rmse")
  pred.xgb <- predict(xgb, data.matrix(test.data))
  
  # Plot real vs predicted values
  png(filename = paste0("res/prediction_", form_station, ".png"))
  plot(test$value, pred.xgb, pch = ".")
  abline(0, 1, col = "red")
  dev.off()
  
  # Print the errors
  print(paste0("MAE: ",  mean(abs(pred.xgb - test.label))))
  print(paste0("RMSE: ", sqrt(mean(abs(pred.xgb - test.label)^2))))
  
  # Error
  res <- data.frame(err = pred.xgb - test.label, real = test.label)
  ggplot(res, aes(real, err)) +
    geom_point(size = .2)
  
  # Plot the feature importance
  model <- xgb.dump(xgb, with.stats=TRUE)
  names <- dimnames(data.matrix(train.data))[[2]]
  importance.matrix <- xgb.importance(names, model=xgb)
  gp <- xgb.plot.importance(importance.matrix)
  
  png(filename = paste0("res/importance_", form_station, ".png"))
  print(gp)
  dev.off()
  
  # Save the model
  xgb.save(xgb, paste0("res/model_", form_station, ".model"))
  
  # Feature influence
  ggplot()
}


