## German flu forecasting evaluation code
## Nicholas Reich
## April 2019

library(ForecastFramework)

## source model code
## TODO: need to generalize reference to model$new() below 
source("models/ContestModel.R")
source("models/sarimaTD-model.R")

### load testing data
testing_data <- readRDS("data/testing_data.rds")
testing_seasons <- paste0(2016:2018, "/", 2017:2019)
first_season_for_fitting <- "2010/2011"

## TODO: REMOVE once the whole script is working
testing_data$subset(rows=13:15)

### testing process for once-fit model
## foreach season s in testing seasons
for(i in 1:length(testing_seasons)) {
    this_season <- testing_seasons[i]
    last_season <- paste0(as.numeric(substr(this_season, 1, 4))-1, "/", substr(this_season, 1, 4))
    
    ## subset data up to (not inclusive) week one of season s
    first_fitting_week <- list(season=first_season_for_fitting, season.week=1)
    last_fitting_week <- list(season=this_season, season.week=6) ## the week at which data was forecasted
    first_col_idx <- which(testing_data$colData$season.week==first_fitting_week$season.week & testing_data$colData$season==first_fitting_week$season)
    last_col_idx <- which(testing_data$colData$season.week==last_fitting_week$season.week & testing_data$colData$season==last_fitting_week$season)
    testing_data$subset(cols = first_col_idx:last_col_idx)
    
    ##  initialize model
    this_model <- SARIMATD1Model$new()
    
    ##  fit model to data
    this_model$fit(testing_data)
    
    ##   foreach week in season s
    for(sw in 1:52){
        ## make data to forecast from
        ## make forecast
        ## rbind tidy forecast data
    }
}

### testing process for each-fit model
## foreach season s in testing seasons
##   foreach week in season s
##      fit model to data up to (not inclusive) week 1 of testing season s
##      make forecast
##      rbind tidy forecast data


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

