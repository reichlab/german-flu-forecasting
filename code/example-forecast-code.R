#### example forecasting on training data

### load utility script
source("code/forecast-utils.R")

source("models/ContestModel.R")
source("models/sarimaTD-model.R")

### define SARIMA model
nsim <- 1000 # Number of SARIMA simulations 
sarimaTDmodel <- SARIMATD1Model$new(period = 52, nsim = nsim)

### load training data
training_data <- readRDS("data/training_data.rds")

## subset training data
first_training_week <- list(year=2010, epiweek=30)
forecast_timezero <- list(year=2016, epiweek=6) ## the week at which data was forecasted
first_col_idx <- which(training_data$colData$week==first_training_week$epiweek & training_data$colData$year==first_training_week$year)
last_col_idx <- which(training_data$colData$week==forecast_timezero$epiweek & training_data$colData$year==forecast_timezero$year)

training_data$subset(rows=13:16, cols = first_col_idx:last_col_idx)

### fit
sarimaTDmodel$fit(training_data)

### forecast
steps <- 15 # forecast ahead `step` number of weeks
forecast_X <- sarimaTDmodel$forecast(training_data, steps = steps)

### tidy up the data
data_forecasted_from <- gather_data(training_data)
fcast_df <- gather_forecast(forecast_X, timezero = forecast_timezero)

### make a plot
ggplot(data=fcast_df, aes(x=date)) +
    geom_line(data=data_forecasted_from, aes(y=value, color=location)) + 
    geom_point(data=data_forecasted_from, aes(y=value, color=location)) +
    geom_line(aes(y=pred_median), data=fcast_df, color="black") + 
    geom_point(aes(y=pred_median), data=fcast_df, color="black") + 
    geom_ribbon(aes(ymin = pred_50_lb, ymax = pred_50_ub), alpha = 0.2) +
    geom_ribbon(aes(ymin = pred_80_lb, ymax = pred_80_ub), alpha = 0.2) +
    geom_ribbon(aes(ymin = pred_95_lb, ymax = pred_95_ub), alpha = 0.2) +
    facet_wrap(.~location) +
    scale_x_date(limits=as.Date(c("2014-10-01", "2016-06-01")))
