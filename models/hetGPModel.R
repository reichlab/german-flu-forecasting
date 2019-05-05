#
# ForecastFramework model definition for
# the heteroskedastic Gaussian process model
# developed in Johnson et al. 2017 
# (https://arxiv.org/abs/1702.00261)
# 
# Integrated into ForecastFramework by
# Herb Susmann, Heather Weaver, Elliot Yang

library(ForecastFramework)
library(R6)
library(tidyverse)
library(hetGP)

source("R/ContestModel.R")

HetGPModel <- R6Class(
  inherit = ContestModel,
  private = list(
    .data = NULL,        ## every model should have this
    .models = list(),    ## specific to models that are fit separately for each location
    .nsim = 1000,        ## models that are simulating forecasts need this
    .tidy_data = tibble(),
    .mle = NULL,
    .transform = function(x) {
      #ifelse(x >= 0, sqrt(x + 1) - 1, log(x + 1))
      log(x + 1)
    },
    .transform_inv = function(y) {
      exp(y) - 1
      #ifelse(y >= 0, (y + 1)^2 - 1, exp(y) - 1)
    },
    .covariates = function(dat, sev = NULL) {
      mat <- dat %>%
        mutate(time = 1:n())
      
      if(!is.null(sev)) {
        mat$severity <- sev
      }
     
      mat %>%
        select(season.week.adj, severity) %>% #, sinewave, severity, time) %>%
        as.matrix
    },
    .severity = function(dat) {
      peaks <- dat %>%
        group_by(season) %>%
        summarize(peak = max(value))
      
      breaks = quantile(peaks$peak, c(1/3, 2/3))
      peaks <- peaks %>%
        mutate(severity = case_when(
          peak < breaks[1] ~ -1,
          peak < breaks[2] ~ 0,
          TRUE ~ 1,
        ))
      
      dat %>%
        left_join(peaks)
    },
    .tidy_incidence_matrix = function(data) {
      colData <- bind_cols(data$colData) %>%
        mutate(key = names(data$colData$season.week),
               season.week.adj = season.week / 52,
               sinewave = 0.5 * (1 - cos((season.week - 1) * 2 * pi / 52)))
      
      as_tibble(data$mat) %>%
        mutate(state = rownames(data$mat)) %>%
        gather(key, value, -starts_with("state")) %>%
        left_join(colData) %>%
        mutate(y = private$.transform(value)) %>%
        #mutate(y = value) %>%
        select(-key)
    },
    .stan_data = function(data) {
      N <- nrow(data)
      y <- data$y
      X <- private$.covariates(data, sev = data$severity)
      K <- ncol(X)
      
      list(
        N = N,
        K = K,
        X = X,
        y = y
      )
    },
    .fit = function(data) {
      tidy_data <- private$.tidy_incidence_matrix(data)
      tidy_data <- private$.severity(tidy_data) # Add severity estimates
      data <- private$.stan_data(tidy_data)
      fit <- mleHetGP(data$X, data$y, covtype = "Gaussian", lower = 10^(-4), upper = 50, settings = list(initStrategy = "smoothed"))
      rebuild(fit, robust = TRUE)
    },
    .forecast = function(newdata, fit, steps) {
      newdata_tidy <- private$.tidy_incidence_matrix(newdata)
      newdata_tidy <- private$.severity(newdata_tidy)
      stan_data <- private$.stan_data(newdata_tidy)
      
      start_season_week <- tail(newdata$colData$season.week, 1)
      season_weeks_to_forecast <- (start_season_week + 1:steps - 1) %% 52 + 1
      
      newdata_df <- tibble(
        season.week = season_weeks_to_forecast,
      ) %>%
        mutate(season.week.adj = season.week / 52,
               sinewave = 0.5 * (1 - cos((season.week - 1) * 2 * pi / 52)),
               severity = 1)
      
      newX <- private$.stan_data(newdata_df)$X
      prediction <- predict(fit, x = newX, xprime = newX)
      
      MASS::mvrnorm(private$.nsim, prediction$mean, prediction$cov + diag(prediction$nugs, nrow(prediction$cov)))
    }
  ),
  public = list(
    ## data will be MatrixData
    fit = function(data) {
      private$.data <- IncidenceMatrix$new(data)
      
      for(i in 1:private$.data$nrow) {
        data_subset <- private$.data$subset(rows = i, mutate = FALSE)
        private$.models[[i]] <- private$.fit(data_subset)
      }
    },
    forecast = function(newdata = private$.data, steps) {
      sim_forecasts <- array(dim = c(length(private$.models), steps, private$.nsim))
      
      dimnames(sim_forecasts) <- list(newdata$rnames, 1:steps, NULL)
      
      for(i in 1:private$.data$nrow) {
        data_subset <- newdata$subset(rows = i, mutate = FALSE)
        sim_forecasts[i, ,] <- t(private$.forecast(data_subset, private$.models[[i]], steps))
      }
      
      private$output <- SimulatedIncidenceMatrix$new(private$.transform_inv(sim_forecasts))
      return(IncidenceForecast$new(private$output, forecastTimes = rep(TRUE, steps)))
    },
    initialize = function(nsim=1000) { 
      private$.nsim = nsim
    },
    fit_params = function(model) {
      private$.models[[model]]$par[c("alpha", "rho[1]", "rho[2]", "rho[3]", "rho[4]", "sigma")]
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
    fit_once = function(value) {
      return(TRUE)
    }
  )
)
