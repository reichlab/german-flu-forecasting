## German flu forecasting evaluation code
## Nicholas Reich
## April 2019

rm(list = ls()) ## protecting against loading stray .RData files

## TO USE THIS CODE, PLEASE :
##  1. DEFINE THE MODEL_ABBR TO BE THE NAME OF YOUR MODEL
##  2. ENSURE THAT YOUR MODEL FOLLOWS THE GUIDELINES IN models/README.md

MODEL_ABBR <- "hetGPModel"

library(ForecastFramework)
library(dplyr)
source("code/forecast-utils.R") ## helper functions for tidying forecast data

STEPS <- 6

## source model code 
source("models/ContestModel.R")
filename <- paste0("models/", MODEL_ABBR, ".R")
source(filename)

### load testing data
testing_data <- readRDS("data/testing_data.rds")
testing_seasons <- paste0(2016:2018, "/", 2017:2019)
first_season_for_fitting <- "2010/2011"

## TODO: REMOVE once the whole script is working
## testing_data$subset(rows=13:15)

eval(parse(text=paste0("this_model <- ", MODEL_ABBR, "$new()")))

### testing evaluation for once-fit model
if(this_model$fit_once){
    last_season <- "2015/2016"
    for(season in testing_seasons){
        ## subset data up to (not inclusive) week one of season s
        first_fitting_week <- list(season=first_season_for_fitting, season.week=1)
        last_fitting_week <- list(season=last_season, season.week=52)
        first_col_idx <- which(testing_data$colData$season.week==first_fitting_week$season.week & testing_data$colData$season==first_fitting_week$season)
        last_col_idx <- which(testing_data$colData$season.week==last_fitting_week$season.week & testing_data$colData$season==last_fitting_week$season)
        once_testing_data <- testing_data$subset(cols = first_col_idx:last_col_idx, mutate = FALSE)
        
        ##  initialize model
        eval(parse(text=paste0("this_model <- ", MODEL_ABBR, "$new()")))
        
        ##  fit model to data
        this_model$fit(once_testing_data)
        
        ##   foreach week in season s, make forecast and save
        for(sw in 1:52){ 
            ## make data to forecast from
            last_col_idx <- which(testing_data$colData$season.week==sw & testing_data$colData$season==season)
            tmp_timezero <- testing_data$colData$week.end.date[last_col_idx]
            tmp_forecast_data <- testing_data$subset(cols = first_col_idx:last_col_idx, mutate = FALSE)
            
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
        last_season <- season
    }
}

if(!this_model$fit_once){
    ### testing process for each-fit model
    last_season <- "2015/2016"
    for(season in testing_seasons){
        ## establish the first week to fit the model to
        first_fitting_week <- list(season=first_season_for_fitting, season.week=1)
        last_fitting_week <- list(season=last_season, season.week=52)
        first_col_idx <- which(testing_data$colData$season.week==first_fitting_week$season.week & testing_data$colData$season==first_fitting_week$season)
        
        ##   foreach week in season s, 
        ##      subset data, fit model, make forecast and append
        for(sw in 1:52){ 
            
            # last week for fitting model (MINUS 1 TO BE NOT INCLUSIVE!)
            last_col_idx <- which(testing_data$colData$season.week==sw & testing_data$colData$season==season) - 1
            each_testing_data <- testing_data$subset(cols = first_col_idx:last_col_idx, mutate = FALSE)
            
            ##  initialize model
            eval(parse(text=paste0("this_model <- ", MODEL_ABBR, "$new()")))
            
            ##  fit model to data
            this_model$fit(each_testing_data)
            
            ## make data to forecast from
            tmp_timezero <- list(
                year = testing_data$colData$year[last_col_idx], 
                epiweek = testing_data$colData$week[last_col_idx] 
            )
            
            ## make forecast
            tmp_forecast <- this_model$forecast(each_testing_data, steps=STEPS)
            
            ## create, rbind tidy forecast data
            tmp_fcast_data <- gather_forecast(tmp_forecast, tmp_timezero)
            if(exists("fcast_data")) {
                fcast_data <- bind_rows(fcast_data, tmp_fcast_data)
            } else {
                fcast_data <- tmp_fcast_data
            }
        }
        last_season <- season
    }
}

tmp_filename <- paste0("results/", MODEL_ABBR, "-testing-results.csv")
write.csv(fcast_data, file=tmp_filename, quote=FALSE, row.names = FALSE)

