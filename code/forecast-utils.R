## helper function for moving single forecast to visualization-ready forecast

gather_forecast <- function(fcast, timezero) {
    require(dplyr)
    require(tidyr)
    require(MMWRweek)
    preds_df <- data.frame(as.table(t(fcast$data$mat))) %>%
        rename(step = Var1, location = Var2) %>%
        select(-Freq) %>% ## not sure what this column/value represents
        mutate(
            step = as.numeric(step),
            pred_median = as.vector(fcast$median(na.rm=TRUE)$mat),
            pred_95_lb = as.vector(fcast$quantile(0.025,na.rm=TRUE)$mat),
            pred_95_ub = as.vector(fcast$quantile(0.975,na.rm=TRUE)$mat),
            pred_80_lb = as.vector(fcast$quantile(0.05,na.rm=TRUE)$mat),
            pred_80_ub = as.vector(fcast$quantile(0.95,na.rm=TRUE)$mat),
            pred_50_lb = as.vector(fcast$quantile(0.25,na.rm=TRUE)$mat),
            pred_50_ub = as.vector(fcast$quantile(0.75,na.rm=TRUE)$mat)
        ) ##%>% 
    ##    gather(metric, value, -step, -location)
    preds_df$date <- MMWRweek2Date(rep(timezero$year, nrow(preds_df)), timezero$epiweek+preds_df$step)
    return(preds_df) 
}

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