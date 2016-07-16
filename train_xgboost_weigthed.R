##########################################################################
# Luz Frias, 2016-06-12
# Train the model
##########################################################################

library(data.table)
library(lubridate)
library(randomForest)
library(xgboost)
library(ggplot2)
library(glmnet)
library(caret)

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
air.data[, wday := wday(date)]

# Feature normalization
air.data[, m30_density := m30_density / 1e6]

# Feature selection
prep.data <- air.data[valid == TRUE & var_formula == "NO2"]
prep.data <- prep.data[, temp_range := max_temp - min_temp]
prep.data[, formula_station := paste(var_formula, station, sep = "_")]
prep.data <- prep.data[, c("date", "formula_station", "hour", "temp_range", "mean_temp", "wind_speed",
                           "rel_humidity_pct", "value", "wday", "m30_density"),
                       with = FALSE]

# Train a model for every station
for (form_station in unique(prep.data$formula_station)) {
  print(paste0("Training ", form_station))
  data <- prep.data[formula_station == form_station ]
  data[, formula_station := NULL]
  
  # Train and test sets
  train <-  data[date < as.Date("2015-01-07")]
  test <- data[date >= as.Date("2015-01-07")]
  test.date <- test$date
  test[, date := NULL]
  train[, date := NULL]
  
  # Split the sets by data and label
  train.label <- train$value
  train.data <- train[, -c("value"), with = FALSE]
  test.label <- test$value
  test.data <- test[, -c("value"), with = FALSE]
  
  custom.metric <- function (data, lev = NULL, model = NULL) {
    pred <- data[, "pred"]
    obs <- data[, "obs"]
    out <- mean(abs(obs - pred) * obs)
    names(out) <- "meanWeightedError"
    out
  }
  
  xgb.control <- trainControl(method = "cv", number = 20, p = 0.075, summaryFunction = custom.metric,
                              allowParallel = TRUE)
  
  # xgb.grid <- expand.grid(nrounds = seq(500, 1500, 1000),
  #                         eta = seq(0.1,0.3,0.1),
  #                         max_depth = seq(10,16,2),
  #                         colsample_bytree = 1,
  #                         min_child_weight = 1,
  #                         gamma = 0)
  
  # Already tuned
  xgb.grid <- expand.grid(nrounds = 1500,
                          eta = 0.2,
                          max_depth = 10,
                          colsample_bytree = 1,
                          min_child_weight = 1,
                          gamma = 0)
  
  set.seed(1234)
  xgb.tune <-train(data.matrix(train.data),
                   train.label,
                   method    ="xgbTree",
                   trControl = xgb.control,
                   tuneGrid  = xgb.grid,
                   verbose   = TRUE,
                   metric    = "meanWeightedError",
                   maximize  = FALSE,
                   nthread   = 8)
  
  xgb <- xgb.tune$finalModel
  
  # Prediction
  pred.xgb <- pmax(0, predict(xgb, data.matrix(test.data)))
  
  # Plot real vs predicted values
  png(filename = paste0("res/xgb_weigthed_prediction_", form_station, ".png"))
  plot(test$value, pred.xgb, pch = ".")
  abline(0, 1, col = "red")
  dev.off()
  
  # Print the errors
  print(paste0("XGB weigthed MAE ",  form_station, ": ", mean(abs(pred.xgb - test.label))))
  print(paste0("XGB weigthed RMSE ", form_station, ": ", sqrt(mean(abs(pred.xgb - test.label)^2))))
  
  # Get better and worse fitted weeks
  hour(test.date) <- test$hour
  res <- data.table(date = test.date, 
                    err = pred.xgb - test.label, real = test.label, pred = pred.xgb)
  res[, week := as.character(floor_date(date, unit = "week"))]
  res[, week.error := sum(abs(err)), by = "week"]
  setkey(res, week.error, date)
  ordered.weeks <- res[, week, by ="week"]$week
  res[, week.f := factor(week, levels = ordered.weeks)]
  ggplot(res, aes(date)) +
    geom_line(aes(y = pred, colour = "pred")) +
    geom_line(aes(y = real, colour = "real")) +
    facet_wrap( ~ week.f, ncol = 1, scales = "free_x")
  ggsave(paste0("res/xgb_weigthed_", form_station, ".png"), height = 200, width = 17,
         units = "cm", limitsize = FALSE)
  
  # Plot the feature importance
  model <- xgb.dump(xgb, with.stats=TRUE)
  names <- dimnames(data.matrix(train.data))[[2]]
  importance.matrix <- xgb.importance(names, model=xgb)
  gp <- xgb.plot.importance(importance.matrix)
  
  png(filename = paste0("res/xgb_weigthed_importance_", form_station, ".png"))
  print(gp)
  dev.off()
  
  # Save the model
  xgb.save(xgb, paste0("res/xgb_weigthed_model_", form_station, ".model"))
}
