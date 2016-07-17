##########################################################################
# Luz Frias, 2016-06-12
# Train the model
##########################################################################

library(data.table)
library(lubridate)
library(randomForest)
library(ggplot2)
library(caret)

source("R/air-data.R")
source("R/meteo-data.R")
source("R/traffic-data.R")
source("R/calendar-data.R")

# Merge air, variable, meteo and traffic data
air.data <- load_historical_air_data()
var.data <- load_contamination_variables()
meteo.data <- load_meteo_data()
#traffic.data <- load_traffic_data()
#calendar.data <- load_calendar_data()
air.data <- merge(air.data, var.data, by.x = "variable", by.y = "var_id")
air.data <- merge(air.data, meteo.data, by = c("date","hour"))
#air.data <- merge(air.data, traffic.data, by = "date")
#air.data <- merge(air.data, calendar.data, by = "date")

setkey(air.data, var_formula, station, date, hour)

# Feature normalization
#air.data[, m30_density := m30_density / 1e6]

prep.data <- air.data[valid == TRUE & var_formula == "NO2"]

# Feature generation
prep.data[, wday  := as.factor(wday(date))]
prep.data[, month := as.factor(month(date))]
prep.data[, temp_range := max_temp - min_temp]
prep.data[, q_95  := quantile(value, .95), by = c("var_formula", "station", "date")]
prep.data[, prewarning := value > 180 & shift(value, type = "lag") > 180, by = c("var_formula", "station")]
prep.data[,    warning := value > 200 & shift(value, type = "lag") > 200, by = c("var_formula", "station")]

# Aggregate by day
daily.data <- prep.data[, c("date", "var_formula", "station", "temp_range", "mean_temp", "min_temp", "month",
                            "max_temp","wind_speed","rel_humidity_pct", "wday", "rain", "q_95"),
                        with = FALSE]
setkey(daily.data, var_formula, station, date)
daily.data <- unique(daily.data)

danger.days <- prep.data[, .(prewarning = any(prewarning),
                             warning = any(warning)),
                         by =  c("var_formula", "station", "date")]
daily.data <- merge(daily.data, danger.days, by = c("var_formula", "station", "date"))
setkey(daily.data, var_formula, station, date)
daily.data[, prev_q_95  := shift(q_95, type = "lag"), by = c("var_formula", "station")]
daily.data <- daily.data[!is.na(prev_q_95)]
  
# Feature selection
daily.data[, formula_station := paste(var_formula, station, sep = "_")]
daily.data <- daily.data[, c("date", "formula_station", "temp_range", "mean_temp", "min_temp", "rain", "month",
                             "max_temp","wind_speed", "rel_humidity_pct", "wday", "prewarning", "warning",
                             "prev_q_95"),
                         with = FALSE]

# Train a model for every station
variables <- c(quote(prewarning), quote(warning)) 
for (col in variables) {
  
  probs <- data.frame(formula_station = character(0), prob = numeric(0),
                      precision = numeric(0), recall = numeric(0))
  
  for (form_station in unique(daily.data[date > as.Date("2014-01-07")]$formula_station)) {
    print(paste0("Training ", as.character(col), " for ", form_station))
    data <- daily.data[formula_station == form_station ]
    data[, formula_station := NULL]
    data[, prewarning := as.factor(as.numeric(prewarning))]
    data[,    warning := as.factor(as.numeric(warning))]
    
    # Train and test sets
    train <-  data[date < as.Date("2014-01-07")]
    test <-   data[date >= as.Date("2014-01-07")]
    
    # Prepare for undersampling to balance the classes
    train.p.pos <- train[eval(col) == "1"]
    train.p.neg <- train[eval(col) == "0"]
    
    if (nrow(train.p.pos) == 0) {
      next
    }
    
    models <- 25
    ntrees  <- 750
    for (i in 1:models) {  
      set.seed(1234+i)
      print(paste0("model ", i))
      train.p.neg.bal <- train.p.neg[sample(1:nrow(train.p.neg), nrow(train.p.pos) * 2),]
      train.data <- rbindlist(list(train.p.pos, train.p.neg.bal))
      train.data <- train.data[sample(nrow(train.data))]
      train.class <- train.data[,eval(col)]
      train.data[, date := NULL]
      train.data[, prewarning := NULL]
      train.data[, warning := NULL]
      
      rf.model.tmp <- randomForest(train.data, y = train.class, method = "class", 
                                   ntree = ntrees, do.trace = FALSE)
      
      png(paste0("res/high_prob_", as.character(col), "_imp_", i, "-", models, ".png"))
      varImpPlot(rf.model.tmp)
      dev.off()
      
      png(paste0("res/high_prob_", as.character(col), "_error_", i, "-", models, ".png"))
      plot(rf.model.tmp)
      dev.off()
      
      if (i == 1) {
        rf.model <- rf.model.tmp
      } else { 
        rf.model <- combine(rf.model.tmp, rf.model)
      }
    }
    
    # Prediction
    pred.rrff <-predict(rf.model, test, type = "prob")
    
    # Plot real vs predicted values
    png(filename = paste0("res/high_prob_", as.character(col), "_pred_", form_station, ".png"))
    plot(test[,eval(col)], pred.rrff[,2])
    dev.off()
    
    # Confusion matrix
    prob.rrff <- pred.rrff[,2]
    calculate.cost <- function(prob) {
      fp <- test[eval(col) == "0" & prob.rrff >= prob]
      fn <- test[eval(col) == "1" & prob.rrff < prob]
      nrow(fp) + nrow(fn) * 4
    }
    to.evaluate <- seq(.5,.95,.05)
    costs <- sapply(to.evaluate, calculate.cost)
    prob <- to.evaluate[which.min(costs)]
    print(table(test[,eval(col)], pred.rrff[,2] > prob))
    tp <- nrow(test[eval(col) == "1" & prob.rrff >= prob])
    fp <- nrow(test[eval(col) == "0" & prob.rrff >= prob])
    fn <- nrow(test[eval(col) == "1" & prob.rrff < prob])
    
    # Save the model
    save(rf.model, file = paste0("res/high_prob_", as.character(col), "_model_", form_station, ".model"))
    
    # and the limit probability
    probs <- rbind(probs, data.frame(formula_station = form_station, prob = prob,
                                     precision = tp / (tp + fp), recall = tp / (tp + fn)))
  }
  
  write.table(probs, file = paste0("res/high_prob_", as.character(col), "_probs.tsv"),
              row.names = FALSE, sep = "\t")
}
