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

# pza españa: 28079004
# Merge air, variable, meteo and traffic data
air.data <- load_historical_air_data()
var.data <- load_contamination_variables()
meteo.data <- load_meteo_data()
traffic.data <- load_traffic_data()
calendar.data <- load_calendar_data()
air.data <- merge(air.data, var.data, by.x = "variable", by.y = "var_id")
air.data <- merge(air.data, meteo.data, by = "date")
air.data <- merge(air.data, traffic.data, by = "date")
air.data <- merge(air.data, calendar.data, by = "date")

# Feature generation
setkey(air.data, var_formula, station, date, hour)
# empeoran!
air.data[, prev_1d_value  := shift(value, 24 * 1,  type = "lag"), by = .(var_formula, station)]
#air.data[, prev_3d_value  := shift(value, 24 * 3,  type = "lag"), by = .(var_formula, station)]
#air.data[, prev_5d_value  := shift(value, 24 * 5,  type = "lag"), by = .(var_formula, station)]
#air.data[, prev_10d_value := shift(value, 24 * 10, type = "lag"), by = .(var_formula, station)]

air.data[, wday := wday(date)]

# Feature selection
prep.data <- air.data[valid == TRUE & var_formula == "NO2"]
prep.data[, formula_station := paste(var_formula, station, sep = "_")]
prep.data <- prep.data[, c("date", "formula_station", "hour", "max_temp", "min_temp", "mean_temp", "wind_speed",
                           "rel_humidity_pct", "value", "is_holiday"),
                       with = FALSE]

# Train a model for every station and variable
for (form_station in unique(prep.data$formula_station)) {
  print(paste0("Training ", form_station))
  data <- prep.data[formula_station == form_station ]
  data[, formula_station := NULL]
  
  data[, week.code := as.character(floor_date(date, unit = "week"))]
  
  # Train and test sets
  set.seed(1234)
  #ind.test <- sample(1:nrow(data), nrow(data) * 0.2)
  #test <- data[ind.test]
  #train <- data[-ind.test]
  all.weeks <- unique(data$week.code)
  test.weeks <- all.weeks[sample(1:length(all.weeks), length(all.weeks) * 0.2)]
  test <-  data[  week.code %in% test.weeks]
  train <- data[!(week.code %in% test.weeks)]
  #train <- train[seq(1, nrow(train)/24, 24) + sample(0:23, nrow(train)/24, replace = TRUE)]
  test.week.label <- test$week.code
  test.date <- test$date
  test[, date := NULL]
  test[, week.code := NULL]
  train[, date := NULL]
  train[, week.code := NULL]
  
  # Training with xgboost
  train.label <- train$value
  train.data <- train[, -c("value"), with = FALSE]
  test.label <- test$value
  test.data <- test[, -c("value"), with = FALSE]
  xgb <- xgboost(data = data.matrix(train.data), label = train.label, max.depth = 4, nthreads = 8,
                 nrounds = 20, objetive = "reg:linear", eval_metric = "rmse", subsample = .075)
  
  train.glm <- copy(train.data)
  test.glm <-  copy(test.data)
  train.glm[, hour := as.factor(hour)]
  test.glm[,  hour := as.factor(hour)]
  glm.mod <- glmnet(model.matrix(~ ., data = train.glm), train.label, family="gaussian")
  
  pred.xgb <- predict(xgb, data.matrix(test.data))
  pred.glm <- predict(glm.mod, model.matrix(~ ., data = test.glm), type = "response", s=c(1))
  
  rf <- randomForest(value ~ ., data = train, ntree = 100, do.trace = TRUE, sampsize=nrow(train)*0.075)
  varImpPlot(rf)
  plot(rf)
  pred.rf <- predict(rf, test.data)
  # Plot real vs predicted values
  #png(filename = paste0("res/prediction_", form_station, ".png"))
  #plot(test$value, pred.xgb, pch = ".")
  #abline(0, 1, col = "red")
  #dev.off()
  
  # Print the errors
  print(paste0("RFF MAE: ",  mean(abs(pred.rf - test.label))))
  print(paste0("RFF RMSE: ", sqrt(mean(abs(pred.rf - test.label)^2))))
  print(paste0("XGB MAE: ",  mean(abs(pred.xgb - test.label))))
  print(paste0("XGB RMSE: ", sqrt(mean(abs(pred.xgb - test.label)^2))))
  print(paste0("GLM MAE: ",  mean(abs(pred.glm - test.label))))
  print(paste0("GLM RMSE: ", sqrt(mean(abs(pred.glm - test.label)^2))))
  
  # Get better and worse fitted weeks
  hour(test.date) <- test$hour
  res <- data.table(date = test.date, week = test.week.label,
                    err = pred.xgb - test.label, real = test.label, pred = pred.xgb)
  res[, week.error := sum(abs(err)), by = "week"]
  setkey(res, week.error, date)
  ordered.weeks <- res [, week, by ="week"]$week
  res[, week.f := factor(week, levels = ordered.weeks)]
  ggplot(res, aes(date)) +
    geom_line(aes(y = pred, colour = "pred")) +
    geom_line(aes(y = real, colour = "real")) +
    facet_wrap( ~ week.f, ncol = 1, scales = "free_x")
  
  
  res.glm <- data.table(date = test.date, week = test.week.label,
                        err = pred.glm[,1] - test.label, real = test.label, pred = pred.glm[,1])
  res.glm[, week.error := sum(abs(err)), by = "week"]
  setkey(res.glm, week.error, date)
  ordered.weeks <- res.glm [, week, by ="week"]$week
  res.glm[, week.f := factor(week, levels = ordered.weeks)]
  ggplot(res.glm, aes(date)) +
    geom_line(aes(y = pred, colour = "pred")) +
    geom_line(aes(y = real, colour = "real")) +
    facet_wrap( ~ week.f, ncol = 1, scales = "free_x")
  
  # Error
  res <- data.frame(err = pred.xgb - test.label, real = test.label)
  #ggplot(res, aes(real, err)) +
  #  geom_point(size = .2)
  
  # Plot the feature importance
  model <- xgb.dump(xgb, with.stats=TRUE)
  names <- dimnames(data.matrix(train.data))[[2]]
  importance.matrix <- xgb.importance(names, model=xgb)
  gp <- xgb.plot.importance(importance.matrix)
  
  #png(filename = paste0("res/importance_", form_station, ".png"))
  print(gp)
  #dev.off()
  
  # Save the model
  #xgb.save(xgb, paste0("res/model_", form_station, ".model"))
  
}

 
