#' helper function for moving single forecast to visualization-ready forecast
#'
#' @param fcast a forecast object from ForecastFramework
#' @param timezero a length-1 vector with the date of the timezero of the forecast
#'
#' @return a tidy data.frame with forecast data
gather_forecast <- function(fcast, timezero) {
    require(dplyr)
    require(tidyr)
    preds_df <- data.frame(as.table(t(fcast$data$mat))) %>%
        rename(step = Var1, location = Var2) %>%
        select(-Freq) %>% ## not sure what this column/value represents
        mutate(
            timezero = timezero,
            step = as.numeric(step),
            date = timezero + 7*step,
            ## the apply and quantile functions below each return a locations x steps matrix
            ## using as.vector(t(...)) returns a vector that has each location with steps 1-6
            ## probably would be better to do these computations separately and bind together, to ensure no mismatches
            pred_median = as.vector(t(apply(fcast$data$arr, FUN=function(x) median(x, na.rm=TRUE), MARGIN = c(1,2)))),
            pred_95_lb = as.vector(t(fcast$quantile(0.025,na.rm=TRUE)$mat)),
            pred_80_lb = as.vector(t(fcast$quantile(0.05,na.rm=TRUE)$mat)),
            pred_50_lb = as.vector(t(fcast$quantile(0.25,na.rm=TRUE)$mat)),
            pred_50_ub = as.vector(t(fcast$quantile(0.75,na.rm=TRUE)$mat)),
            pred_80_ub = as.vector(t(fcast$quantile(0.95,na.rm=TRUE)$mat)),
            pred_95_ub = as.vector(t(fcast$quantile(0.975,na.rm=TRUE)$mat)),
            na_value_count = as.vector(t(apply(fcast$data$arr, FUN=function(x) sum(is.na(x)), MARGIN = c(1,2))))
        )
    # preds_df$timezero <- MMWRweek2Date(timezero$year, timezero$epiweek)
    # preds_df$date <- MMWRweek2Date(rep(timezero$year, nrow(preds_df)), timezero$epiweek+preds_df$step)
    return(preds_df) 
}

#' make tidy data out of an IncidenceMatrix object
#'
#' @param incmat an IncidenceMatrix object
#'
#' @return a tidy dataframe with columns for location, value, year, epiweek, date
gather_data <- function(incmat) {
    require(dplyr)
    require(tidyr)
    require(MMWRweek)
    incdat <- data.frame(as.table(t(incmat$mat))) %>%
        rename(date = Var1, location = Var2, value=Freq) %>%
        mutate(date = as.Date(date),
               season = rep(incmat$colData$season, incmat$nrow),
               season_week = rep(incmat$colData$season.week, incmat$nrow))
    return(incdat) 
}