## training results 

library(ForecastFramework)
library(tidyverse)

source("code/forecast-utils.R")

training_data <- readRDS("data/training_data.rds")
data_forecasted_from <- gather_data(training_data)

data_forecasted <- read_csv("results/sarima-training-results.csv") %>%
#data_forecasted <- fcast_data %>%
    left_join(dplyr::select(data_forecasted_from, location, date, value)) %>%
    rename(truth=value) %>%
    mutate(
        bias = pred_median - truth,
        ci50_cov = truth>pred_50_lb & truth<pred_50_ub,
        ci80_cov = truth>pred_80_lb & truth<pred_80_ub,
        ci95_cov = truth>pred_95_lb & truth<pred_95_ub)

## overall summary
colMeans(dplyr::select(data_forecasted, bias, starts_with("ci")), na.rm=TRUE)

## by_region_summary
data_forecasted %>% group_by(location) %>%
    summarize(
        avg_bias = mean(bias, na.rm=TRUE), 
        ci50_cov = mean(ci50_cov, na.rm=TRUE),
        ci80_cov = mean(ci80_cov, na.rm=TRUE),
        ci95_cov = mean(ci95_cov, na.rm=TRUE))

### make plots

## forecasts from 2015-11-08, epiweek 45
ggplot(data=filter(data_forecasted, timezero==as.Date("2015-11-08")), aes(x=date)) +
    geom_line(data=data_forecasted_from, aes(y=value)) + 
    geom_point(data=data_forecasted_from, aes(y=value)) +
    geom_line(aes(y=pred_median), color="red") + 
    geom_point(aes(y=pred_median), color="red") + 
    geom_ribbon(aes(ymin = pred_50_lb, ymax = pred_50_ub), alpha = 0.2, fill="red") +
    geom_ribbon(aes(ymin = pred_80_lb, ymax = pred_80_ub), alpha = 0.2, fill="red") +
    geom_ribbon(aes(ymin = pred_95_lb, ymax = pred_95_ub), alpha = 0.2, fill="red") +
    facet_wrap(.~location) +
    scale_x_date(limits=as.Date(c("2015-10-01", "2016-06-01"))) +
    ggtitle("Forecasts from 2015 epiweek 45")


## forecasts from 2016-01-17, epiweek 3
ggplot(data=filter(data_forecasted, timezero==as.Date("2016-01-17")), aes(x=date)) +
    geom_line(data=data_forecasted_from, aes(y=value)) + 
    geom_point(data=data_forecasted_from, aes(y=value)) +
    geom_line(aes(y=pred_median), color="red") + 
    geom_point(aes(y=pred_median), color="red") + 
    geom_ribbon(aes(ymin = pred_50_lb, ymax = pred_50_ub), alpha = 0.2, fill="red") +
    geom_ribbon(aes(ymin = pred_80_lb, ymax = pred_80_ub), alpha = 0.2, fill="red") +
    geom_ribbon(aes(ymin = pred_95_lb, ymax = pred_95_ub), alpha = 0.2, fill="red") +
    facet_wrap(.~location) +
    scale_x_date(limits=as.Date(c("2015-10-01", "2016-06-01"))) +
    ggtitle("Forecasts from 2016 epiweek 3")

## forecasts from 2016-03-06, epiweek 10
ggplot(data=filter(data_forecasted, timezero==as.Date("2016-03-06")), aes(x=date)) +
    geom_line(data=data_forecasted_from, aes(y=value)) + 
    geom_point(data=data_forecasted_from, aes(y=value)) +
    geom_line(aes(y=pred_median), color="red") + 
    geom_point(aes(y=pred_median), color="red") + 
    geom_ribbon(aes(ymin = pred_50_lb, ymax = pred_50_ub), alpha = 0.2, fill="red") +
    geom_ribbon(aes(ymin = pred_80_lb, ymax = pred_80_ub), alpha = 0.2, fill="red") +
    geom_ribbon(aes(ymin = pred_95_lb, ymax = pred_95_ub), alpha = 0.2, fill="red") +
    facet_wrap(.~location) +
    scale_x_date(limits=as.Date(c("2015-10-01", "2016-06-01"))) +
    ggtitle("Forecasts from 2016 epiweek 10")

## forecasts from 2016-04-10, epiweek 15
ggplot(data=filter(data_forecasted, timezero==as.Date("2016-04-10")), aes(x=date)) +
    geom_line(data=data_forecasted_from, aes(y=value)) + 
    geom_point(data=data_forecasted_from, aes(y=value)) +
    geom_line(aes(y=pred_median), color="red") + 
    geom_point(aes(y=pred_median), color="red") + 
    geom_ribbon(aes(ymin = pred_50_lb, ymax = pred_50_ub), alpha = 0.2, fill="red") +
    geom_ribbon(aes(ymin = pred_80_lb, ymax = pred_80_ub), alpha = 0.2, fill="red") +
    geom_ribbon(aes(ymin = pred_95_lb, ymax = pred_95_ub), alpha = 0.2, fill="red") +
    facet_wrap(.~location) +
    scale_x_date(limits=as.Date(c("2015-10-01", "2016-06-01"))) +
    ggtitle("Forecasts from 2016 epiweek 15")



