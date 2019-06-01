library(ForecastFramework)
library(R6)
library(forecast)
library(sarimaTD)
library(reticulate)

## TO INSTALL
## install python 3
## install pip
## install numpy using pip
## install torch using pip
## install 

#SET THIS TO YOUR PYTHON VERSION
use_python("/usr/local/bin/python")
source_python("./models/pyfiles/main.py")



DL4EPI <- R6Class(
  inherit = ContestModel,
  private = list(
    .data = NULL,        ## every model should have this
    .models = list(),    ## specific to models that are fit separately for each location
    .nsim = 1000,        ## models that are simulating forecasts need this
    .STEPS = 6,
    .fit_once = TRUE 
  ),
  public = list(
    setSteps = function(step_ahead){
      private$.STEPS <- step_ahead
    },
    ## data will be MatrixData
    fit = function(data) {
      if("fit" %in% private$.debug){browser()}
      #adjacency_matrix <- "./GER_states_adjacency.txt"
      training_data <- readRDS("./data/training_data.rds")
      adjacency_matrix <- training_data$metaData$adjacency_mat
      adj_mat_fname <- "./GER_states_adjacency.txt"
      
      # write to adjacency matrix to txt file
      write.table(adjacency_matrix, file=adj_mat_fname,sep = ',', 
                  row.names = FALSE, col.names=FALSE)
      
      epochs <- 500
      save_name <- "tmp"
      model_name <- "CNNRNN_Res"
      ## stores data for easy access and checks to make sure it's the right class
      private$.data <- IncidenceMatrix$new(data)
    
      ## for each location/row
      #for (row_idx in 1:private$.data$nrow) {
        ### need to create a y vector with incidence at time t
      #  y <- private$.data$subset(rows = row_idx, mutate = FALSE)
        
        ## convert y vector to time series data type
      #print (private$.data)
      y_ts <- ts(as.vector(private$.data$mat), frequency = private$.period)
      
      
      print (length(y_ts))
      print ("--------")
        ## fit sarimaTD with 'fit_sarima()' from sarimaTD package
        ## fit_sarima() performs box-cox transformation and seasonal differencing
      is_this_nan <- NaN
      for (step_ahead in 1:private$.STEPS){
        while(is.nan(is_this_nan )){
          is_this_nan <- train_dl(y_ts,adj_mat_fname,model_name,save_name,step_ahead,epochs)
        }
        is_this_nan <- NaN
      }
    },
    forecast = function(newdata = private$.data, steps) {
      save_name <- "tmp"
      model_name <- "CNNRNN_Res"
      
      ## include for debugging
      if("forecast" %in% private$.debug){browser()} 
      
      ## number of models (provinces) to forecast
      nmodels <- length(private$.models)
      
      ## define an array to store the simulated forecasts
      #sim_forecasts <- array(dim = c(nmodels, steps, private$.nsim))
      
      adj_mat_fname <- "GER_states_adjacency.txt"
      
      # print (dim(newdata))
      list_of_matrices <- array(rep(NA,newdata$nrow*private$.STEPS*private$.nsim),dim=c(newdata$nrow,private$.STEPS,private$.nsim))
      tmp <- mytest(private$.data$mat,adj_mat_fname,model_name,save_name,1,t(newdata$mat))
      
     
      #for (sims in 1:100){
        for (step_ahead in 1:private$.STEPS){
          tmp <- mytest(private$.data$mat,adj_mat_fname,model_name,save_name,step_ahead,t(newdata$mat))
          for (i_ in 1:length(tmp))
          list_of_matrices[i_,step_ahead,] <- rnorm(private$.nsim,tmp[i_],sd=1)
        }
      #}
     # print (dim(sim_forecasts))
      dimnames(list_of_matrices) <- list(newdata$rnames, 1:steps, NULL)
      
      private$output <- SimulatedIncidenceMatrix$new(list_of_matrices)
      return(IncidenceForecast$new(private$output, forecastTimes = rep(TRUE, steps)))
    },
    initialize = function(period = 52, nsim=1000, STEPS=6) { 
      ## need to store these arguments within the model object
      private$.nsim <- nsim
      private$.period <- period
      private$.STEPS <- STEPS
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
    },
    STEPS = function(value) {
      ## use this form when you want to be able to change this parameter
      private$defaultActive(type="private", ".STEPS", val=value)
    },
    fit_once = function(value) {
      ## use this form when you want this parameter to be un-modifiable
      if(!missing(value))
        stop("Writing directly to the model period is not allowed.")
      return(private$.fit_once)
    }
  ),
  lock_objects = FALSE
  
)
