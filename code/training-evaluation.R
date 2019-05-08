## German flu forecasting evaluation code: training data
## Nicholas Reich
## April 2019

rm(list = ls()) ## protecting against loading stray .RData files

## TO USE THIS CODE, PLEASE :
##  1. DEFINE THE MODEL_ABBR TO BE THE NAME OF YOUR MODEL
##  2. ENSURE THAT YOUR MODEL FOLLOWS THE GUIDELINES IN models/README.md

MODEL_ABBR <- "EmpiricalBayesModel"


library(ForecastFramework)
library(dplyr)
source("code/forecast-utils.R") ## helper functions for tidying forecast data

STEPS <- 6

## source model code 
source("models/ContestModel.R")
filename <- paste0("models/", MODEL_ABBR, ".R")
source(filename)

### load training data
training_data <- readRDS("data/training_data.rds")
initial <- as.matrix(read.csv("data/initials_new.csv"), header = TRUE)
training_data$metaData$initial <- initial
first_season_for_fitting <- "2010/2011"
eval_season <- "2015/2016"
last_season <- "2014/2015"

## TODO: REMOVE once the whole script is working
## training_data$subset(rows=13:15)

eval(parse(text=paste0("this_model <- ", MODEL_ABBR, "$new()")))

### training evaluation for once-fit model
if(this_model$fit_once){
    
    ## subset data up to (not inclusive) week one of season s
    first_fitting_week <- list(season=first_season_for_fitting, season.week=1)
    last_fitting_week <- list(season=last_season, season.week=52)
    first_col_idx <- which(training_data$colData$season.week==first_fitting_week$season.week & training_data$colData$season==first_fitting_week$season)
    last_col_idx <- which(training_data$colData$season.week==last_fitting_week$season.week & training_data$colData$season==last_fitting_week$season)
    once_training_data <- training_data$subset(cols = first_col_idx:last_col_idx, mutate = FALSE)
    
    ##  initialize model
    eval(parse(text=paste0("this_model <- ", MODEL_ABBR, "$new()")))

    ##  fit model to data
    this_model$fit(once_training_data)
    
    ##   foreach week in season s, make forecast and save
    for(sw in 1:52){ 
        ## make data to forecast from
        last_col_idx <- which(training_data$colData$season.week==sw & training_data$colData$season==eval_season)
        tmp_timezero <- training_data$colData$week.end.date[last_col_idx]
        tmp_forecast_data <- training_data$subset(cols = first_col_idx:last_col_idx, mutate = FALSE)
        
        ## make forecast
        tmp_forecast <- this_model$forecast(tmp_forecast_data, steps=STEPS, nSimulations = 30)
        
        ## create, rbind tidy forecast data
        tmp_fcast_data <- gather_forecast(tmp_forecast, tmp_timezero)
        if(exists("fcast_data")) {
            fcast_data <- bind_rows(fcast_data, tmp_fcast_data)
        } else {
            fcast_data <- tmp_fcast_data
        }
    }
}

if(!this_model$fit_once){

    ### training process for each-fit model
    
    ## establish the first week to fit the model to
    first_fitting_week <- list(season=first_season_for_fitting, season.week=1)
    last_fitting_week <- list(season=last_season, season.week=52)
    first_col_idx <- which(training_data$colData$season.week==first_fitting_week$season.week & training_data$colData$season==first_fitting_week$season)

    ##   foreach week in season s, 
    ##      subset data, fit model, make forecast and append
    for(sw in 1:52){ 
        
        # last week for fitting model (MINUS 1 TO BE NOT INCLUSIVE!)
        last_col_idx <- which(training_data$colData$season.week==sw & training_data$colData$season==eval_season) - 1
        each_training_data <- training_data$subset(cols = first_col_idx:last_col_idx, mutate = FALSE)
        
        ##  initialize model
        eval(parse(text=paste0("this_model <- ", MODEL_ABBR, "$new()")))
        
        ##  fit model to data
        this_model$fit(each_training_data)
        
        ## make data to forecast from
        tmp_timezero <- list(
            year = training_data$colData$year[last_col_idx], 
            epiweek = training_data$colData$week[last_col_idx] 
        )

        ## make forecast
        tmp_forecast <- this_model$forecast(each_training_data, steps=STEPS)
        
        ## create, rbind tidy forecast data
        tmp_fcast_data <- gather_forecast(tmp_forecast, tmp_timezero)
        if(exists("fcast_data")) {
            fcast_data <- bind_rows(fcast_data, tmp_fcast_data)
        } else {
            fcast_data <- tmp_fcast_data
        }
    }
}

tmp_filename <- paste0("results/", MODEL_ABBR, "-training-results.csv")
write.csv(fcast_data, file=tmp_filename, quote=FALSE, row.names = FALSE)
