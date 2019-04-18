# seasonal GAM model

library(ForecastFramework)
library(R6)
library(mgcv)

# seasonalGAM Description:
# Implementation of the a generalized additive model cyclic spline model

seasonalGAM <- R6Class(
    inherit = ContestModel,
    private = list(
        .data = NULL,        ## every model should have this
        .models = list(),    ## specific to models that are fit separately for each location
        .nsim = 1000         ## models that are simulating forecasts need this
    ),
    public = list(
        ## data will be MatrixData
        fit = function(data) {
            if("fit" %in% private$.debug){browser()}
            ## stores data for easy access and checks to make sure it's the right class
            private$.data <- IncidenceMatrix$new(data)
            
            ## for each location/row
            for (row_idx in 1:private$.data$nrow) {
                ### need to create a y vector with incidence at time t
                y <- as.vector(private$.data$subset(rows = row_idx, mutate = FALSE)$mat)
                season.week <- private$.data$colData$season.week
                tmp_dat <- data.frame(y=y, season.week=season.week)
                private$.models[[row_idx]] <- gam(y ~ s(season.week, bs="cc"), data=tmp_dat)
            }
        },
        forecast = function(newdata = private$.data, steps) {
            ## include for debugging
            if("forecast" %in% private$.debug){browser()} 
            
            ## number of models (provinces) to forecast
            nmodels <- length(private$.models)
            
            ## define an array to store the simulated forecasts
            sim_forecasts <- array(dim = c(nmodels, steps, private$.nsim))
            dimnames(sim_forecasts) <- list(newdata$rnames, 1:steps, NULL)
            
            ## iterate through each province and forecast with simulate.satimaTD
            for(model_idx in 1:length(private$.models)) {
                ## code adapted from example at: https://stat.ethz.ch/pipermail/r-help/2011-April/275632.html
                
                n_sim <- private$.nsim
                ## isolate one fitted model and newdata input
                fm <- private$.models[[model_idx]]
                start_season_week <- tail(newdata$colData$season.week, 1)
                season_weeks_to_forecast <- (start_season_week + 1:steps -1) %% 52 +1
                
                ## extract parameter estiamtes and cov matrix...
                beta <- coef(fm)
                Vb <- vcov(fm)
                
                ## simulate replicate beta vectors from posterior...
                Cv <- chol(Vb)
                nb <- length(beta)
                br <- t(Cv) %*% matrix(rnorm(n_sim*nb),nb,n_sim) + beta
                
                ## turn these into replicate linear predictors...
                Xp <- predict(fm,newdata=data.frame(season.week=season_weeks_to_forecast),type="lpmatrix")
                lp <- Xp%*%br
                fv <- lp ## ... finally, replicate expected value vectors
                
                ## now simulate from normal deviates with mean as in fv
                ## and estimated scale...
                tmp <- matrix(rnorm(length(fv), mean=fv, sd=sqrt(fm$sig2)), nrow=nrow(fv), ncol(fv))
                
                sim_forecasts[model_idx, , ] <- tmp
            }
            private$output <- SimulatedIncidenceMatrix$new(sim_forecasts)
            return(IncidenceForecast$new(private$output, forecastTimes = rep(TRUE, steps)))
        },
        initialize = function(nsim=1000) { 
            ## this code is run during SARIMAModel$new()
            ## need to store these arguments within the model object
            private$.nsim <- nsim
        },
        predict = function(newdata) {
            stop("predict method has not been written.")
        }
    ),
    active = list(
        ## This list determines how you can access pieces of your model object
        data = function(value) {
            ## use this form when you want this parameter to be un-modifiable
            if(!missing(value))
                stop("Writing directly to the data is not allowed.")
            return(private$.data)
        },
        models = function(value) {
            ## use this form when you want this parameter to be un-modifiable
            if(!missing(value))
                stop("Writing directly to the models is not allowed.")
            return(private$.models)
        },
        nsim = function(value) {
            ## use this form when you want to be able to change this parameter
            private$defaultActive(type="private", ".nsim", val=value)
        },
        period = function(value) {
            ## use this form when you want this parameter to be un-modifiable
            if(!missing(value))
                stop("Writing directly to the model period is not allowed.")
            return(private$.period)
        }
    )
)