#' helper function for moving single forecast to visualization-ready forecast
#'
#' @param fcast a forecast object from ForecastFramework
#' @param timezero a list with two named scalar quantities: year and epiweek
#'
#' @return a tidy data.frame with forecast data
gather_forecast <- function(fcast, timezero) {
    require(dplyr)
    require(tidyr)
    require(MMWRweek)
    preds_df <- data.frame(as.table(t(fcast$data$mat))) %>%
        rename(step = Var1, location = Var2) %>%
        select(-Freq) %>% ## not sure what this column/value represents
        mutate(
            timezero = MMWRweek2Date(timezero$year, timezero$epiweek),
            step = as.numeric(step),
            date = timezero + 7*step,
            pred_median = as.vector(apply(tmp_forecast$data$arr, FUN=function(x) median(x, na.rm=TRUE), MARGIN =c(1,2))),
            pred_95_lb = as.vector(fcast$quantile(0.025,na.rm=TRUE)$mat),
            pred_80_lb = as.vector(fcast$quantile(0.05,na.rm=TRUE)$mat),
            pred_50_lb = as.vector(fcast$quantile(0.25,na.rm=TRUE)$mat),
            pred_50_ub = as.vector(fcast$quantile(0.75,na.rm=TRUE)$mat),
            pred_80_ub = as.vector(fcast$quantile(0.95,na.rm=TRUE)$mat),
            pred_95_ub = as.vector(fcast$quantile(0.975,na.rm=TRUE)$mat)
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
        rename(textdate = Var1, location = Var2, value=Freq) %>%
        mutate(
            year = as.numeric(substr(textdate, 2, 5)),
            epiweek = as.numeric(substr(textdate, 8, 9)),
            date = MMWRweek::MMWRweek2Date(year, epiweek)
            )
    return(incdat) 
}