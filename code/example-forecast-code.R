## example forecasting on training data

library(RCurl)

# Function of Source Github Models
source_github <- function(u) {
    # read script lines from website and evaluate
    script <- getURL(u, ssl.verifypeer = FALSE)
    eval(parse(text = script),envir=.GlobalEnv)
}  
# Source R6 SARIMATD model files
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/ContestModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/SARIMATD1Model.R')

## define SARIMA model
nsim <- 10 # Number of SARIMA simulations 
sarimaTDmodel <- SARIMATD1Model$new(period = 52, nsim = nsim)

## load training data
training_data <- readRDS("data/training_data.rds")

## fit
sarimaTDmodel$fit(training_data)
steps <- 6 # forecast ahead 52 weeks
forecast_X <- sarimaTDmodel$forecast(training_inc$subset(cols = 1:(training_inc$ncol-26), mutate=FALSE),steps = steps)
preds_df <- data.frame(as.table(t(forecast_X$data$mat))) %>%
    mutate( 
        pred_total_ili = as.vector(forecast_X$median(na.rm=TRUE)$mat),
        pred_95_lb = as.vector(forecast_X$quantile(0.025,na.rm=TRUE)$mat),
        pred_95_ub = as.vector(forecast_X$quantile(0.975,na.rm=TRUE)$mat),
        pred_80_lb = as.vector(forecast_X$quantile(0.05,na.rm=TRUE)$mat),
        pred_80_ub = as.vector(forecast_X$quantile(0.95,na.rm=TRUE)$mat),
        pred_50_lb = as.vector(forecast_X$quantile(0.25,na.rm=TRUE)$mat),
        pred_50_ub = as.vector(forecast_X$quantile(0.75,na.rm=TRUE)$mat)
    )
print(head(preds_df,5)) 

ggplot(data=preds_df, aes(x=1)) +
    geom_ribbon(
        mapping = aes(ymin = pred_95_lb, ymax = pred_95_ub),
        fill = "cornflowerblue",
        alpha = 0.2) +
    geom_ribbon(
        mapping = aes(ymin = pred_80_lb, ymax = pred_80_ub),
        fill = "cornflowerblue",
        alpha = 0.2) +
    geom_ribbon(
        mapping = aes(ymin = pred_50_lb, ymax = pred_50_ub),
        alpha = 0.2) +
    geom_line(
        mapping = aes(y = pred_total_ili),
        color = "royalblue",
        size = 2) 
# +
#     geom_line(aes(x = week_start, y = weighted_ili),
#         size=0.7,
#         data = data_X_3years) +
#     xlab("") + ylab("Weighted ILI")
