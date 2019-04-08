## example forecasting on training data

library(RCurl)

# Function of Source Github Models
source_github <- function(u) {
    # read script lines from website and evaluate
    script <- getURL(u, ssl.verifypeer = FALSE)
    eval(parse(text = script),envir=.GlobalEnv)
}  
# Source R6 SARIMATD model files
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/ContestModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/SARIMATD1Model.R')

## define SARIMA model
nsim <- 100 # Number of SARIMA simulations 
sarimaTDmodel <- SARIMATD1Model$new(period = 52, nsim = nsim)

## load training data
training_data <- readRDS("data/training_data.rds")

training_data$subset(rows=13:16)

## fit
sarimaTDmodel$fit(training_data)

## forecast
steps <- 10 # forecast ahead `step` number of weeks
forecast_timezero <- list(year=2016, epiweek=6) ## the week at which data was forecasted
last_col_idx <- which(training_data$colData$week==forecast_timezero$epiweek & training_data$colData$year==forecast_timezero$year)
forecast_X <- sarimaTDmodel$forecast(
    training_data$subset(cols = 1:last_col_idx, mutate=FALSE),
    steps = steps
    )

data_forecasted_from <- gather_data(training_data$subset(cols = 1:last_col_idx, mutate=FALSE))

fcast_df <- gather_forecast(forecast_X, timezero = forecast_timezero)

ggplot(data=fcast_df, aes(x=date)) +
    geom_line(data=data_forecasted_from, aes(y=value, color=location)) + 
    geom_point(data=data_forecasted_from, aes(y=value, color=location)) +
    geom_line(aes(y=pred_median), data=fcast_df, color="black") + 
    geom_point(aes(y=pred_median), data=fcast_df, color="black") + 
    geom_ribbon(aes(ymin = pred_50_lb, ymax = pred_50_ub), alpha = 0.2) +
    geom_ribbon(aes(ymin = pred_80_lb, ymax = pred_80_ub), alpha = 0.2) +
    geom_ribbon(aes(ymin = pred_95_lb, ymax = pred_95_ub), alpha = 0.2) +
    facet_wrap(.~location) +
    scale_x_date(limits=as.Date(c("2015-10-01", "2016-06-01")))
