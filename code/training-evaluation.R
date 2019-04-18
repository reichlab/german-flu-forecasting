## German flu forecasting evaluation code: training data
## Nicholas Reich
## April 2019

library(ForecastFramework)
library(dplyr)
source("code/forecast-utils.R") ## helper functions for tidying forecast data

## TO USE THIS CODE, PLEASE READ THE FOLLOWING INSTRUCTIONS:

## 1. ADD YOUR MODEL SOURCE CODE BELOW
## 2. SEARCH FOR THE $new CALL AND REPLACE YOUR MODEL NAME THERE
## 3. DOES YOUR MODEL FIT ONCE OR EACH TIME? CHANGE THIS_MODEL_FITS_ONCE APPROPRIATELY.
## 4. CHANGE THE MODEL_ABBR VARIABLE TO BE SOMETHING REASONABLE FOR YOUR MODEL

## source model code 
source("models/ContestModel.R")
#source("models/sarimaTD-model.R")
#source("models/SeasonalMedianModel.R")
source("models/EmpiricalBayesModel.R")

THIS_MODEL_FITS_ONCE <- TRUE
MODEL_ABBR <- "empirical-bayes"

STEPS <- 6

### load training data
training_data <- readRDS("data/training_data.rds")
first_season_for_fitting <- "2010/2011"
eval_season <- "2015/2016"
last_season <- "2014/2015"

## TODO: REMOVE once the whole script is working
## training_data$subset(rows=13:15)

### training evaluation for once-fit model
if(THIS_MODEL_FITS_ONCE){
    
    ## subset data up to (not inclusive) week one of season s
    first_fitting_week <- list(season=first_season_for_fitting, season.week=1)
    last_fitting_week <- list(season=last_season, season.week=52)
    first_col_idx <- which(training_data$colData$season.week==first_fitting_week$season.week & training_data$colData$season==first_fitting_week$season)
    last_col_idx <- which(training_data$colData$season.week==last_fitting_week$season.week & training_data$colData$season==last_fitting_week$season)
    once_training_data <- training_data$subset(cols = first_col_idx:last_col_idx, mutate = FALSE)
    
    ##  initialize model
    ## TODO: need to make this line be automatically called
    this_model <- EmpiricalBayesModel$new()
    
    ##  fit model to data
    this_model$fit(once_training_data)
    
    ##   foreach week in season s, make forecast and save
    for(sw in 1:52){ 
        ## make data to forecast from
        last_col_idx <- which(training_data$colData$season.week==sw & training_data$colData$season==eval_season)
        tmp_timezero <- list(
            year = training_data$colData$year[last_col_idx], 
            epiweek = training_data$colData$week[last_col_idx] 
        )
        tmp_forecast_data <- training_data$subset(cols = first_col_idx:last_col_idx, mutate = FALSE)
        
        ## make forecast
        tmp_forecast <- this_model$forecast(tmp_forecast_data, steps=STEPS)
        
        ## create, rbind tidy forecast data
        tmp_fcast_data <- gather_forecast(tmp_forecast, tmp_timezero)
        if(exists("fcast_data")) {
            fcast_data <- bind_rows(fcast_data, tmp_fcast_data)
        } else {
            fcast_data <- tmp_fcast_data
        }
    }
}

if(!THIS_MODEL_FITS_ONCE){

### training process for each-fit model
## foreach season s in training seasons
##   foreach week in season s
##      fit model to data up to (not inclusive) week 1 of training season s
##      make forecast
##      rbind tidy forecast data
}

tmp_filename <- paste0("results/", MODEL_ABBR, "-training-results.csv")
write.csv(fcast_data, file=tmp_filename, quote=FALSE, row.names = FALSE)
