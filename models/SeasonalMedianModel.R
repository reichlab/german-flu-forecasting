SeasonalMedianModel<-R6Class(
    classname = "SeasonalMedianModel",
    inherit = ForecastModel,
    private = list(
        .medians = matrix(),
        timeInYear = matrix(),
        .nrow = NA,
        .nsim = 1,
        .data = IncidenceMatrix$new(),
        newdata = SimulatedIncidenceMatrix$new(),
        output = SimulatedIncidenceMatrix$new()
    ),
    public = list(
        fit = function(data){
            if("fit" %in% private$.debug){browser()}
            self$prepareFitData(data)
            self$fit_()
        },
        predict = function(newdata){
            if("fit" %in% private$.debug){browser()}
            self$preparePredictData(data)
            self$fit_()
        },
        fit_ = function(){
            if('fit_' %in% private$.debug){
                browser()
            }
            colnames = unique(private$newdata$colData$time.in.year)
            private$.medians = matrix(NA,private$.nrow,length(colnames))
            for(col in colnames){
                locations = private$newdata$colData$time.in.year == col
                private$.medians[,col] =
                    apply(
                        private$newdata$mat[,locations,drop=FALSE],
                        1,
                        function(...){
                            median(...,na.rm=T)
                        }
                    )
            }
            colnames(private$.medians) = colnames
        },
        prepareFitData = function(data){
            if('prepareFitData' %in% private$.debug){
                browser()
            }
            private$.data = data$clone(TRUE)
            private$newdata = private$.data$clone(TRUE)
            private$.nrow = private$.data$nrow
            private$.medians =
                matrix(NA,private$.nrow,private$newdata$metaData$max.year.time)
        },
        preparePredictData = function(newdata){
            if('preparePredictData' %in% private$.debug){
                browser()
            }
            private$newdata = newdata$clone(TRUE)
            ##Move the time in year one step ahead
            private$newdata$colData$time.in.year =
                (
                    (private$newdata$colData$time.in.year) %%
                        private$newdata$metaData$max.year.time
                ) + 1
            if('yr' %in% names(private$newdata$colData)){
                private$newdata$colData$year[private$newdata$time.in.year==1] =
                    private$newdata$colData$year[private$newdata$time.in.year==1] + 1
            }
        },
        predictRow_ = function(row){
            if('predictRow_' %in% private$.debug){
                browser()
            }
            return(self$predict_(col=0)[row,])
        },
        predict_ = function(col=0){
            if('predict_' %in% private$.debug){
                browser()
            }
            ##browser()
            if(any(col> 0)){
                private$output$mutate(
                    cols=col,
                    data=private$.medians[,private$newdata$colData$time.in.year]
                )
            }
            else{
                private$output$mutate(
                    data = private$.medians[,private$newdata$colData$time.in.year]
                )
            }
            return(private$.medians[,private$newdata$colData$time.in.year])
        },
        prepareOutputData = function(inputData,steps){
            if('prepareOutputData' %in% private$.debug){
                browser()
            }
            private$output = SimulatedIncidenceMatrix$new(inputData,1)
            private$output$addColumns(steps)
            private$output$colData$time.in.year[inputData$ncol+1:steps] =
                (
                    private$output$colData$time.in.year[inputData$ncol] + 1:steps
                ) %% private$output$metaData$max.year.time
            private$output$colData$time.in.year[
                private$output$colData$time.in.year == 0
                ] = private$output$metaData$max.year.time
            ##Fix this
            ##if('yr' %in% names(private$newdata$colData)){
            ##  private$newdata$colData$year[private$newdata$time.in.year==1] =
            ##    private$newdata$colData$year[private$newdata$time.in.year==1] + 1
            ##}
        },
        forecast = function(newdata,steps){
            if('forecast' %in% private$.debug){
                browser()
            }
            self$prepareOutputData(newdata,steps)
            private$newdata =
                private$output$subset(cols=newdata$ncol+1:steps,mutate=FALSE)
            self$predict_(col=(newdata$ncol+1):(newdata$ncol+steps))
            private$output$subset(cols=newdata$ncol+1:steps)
            return(
                IncidenceForecast$new(
                    private$output,
                    forecastTimes = rep(TRUE,private$output$ncol)
                )
            )
        },
        peakTime = function(newdata,start,end){
            if("peakTime" %in% private$.debug){browser()}
            private$newdata = IncidenceMatrix$new(newdata)
            dnames = list(
                private$newdata$rnames,
                paste('[',start,',',end,')'),
                NULL
            )
            if(start <= 0){
                private$newdata$tail(1-start)
                prev.max = apply(private$newdata$mat,1,max)
                prev.time = apply(private$newdata$mat,1,which.max)
            } else {
                prev.max = 0
                prev.time = 1
            }
            if(end > 0){
                self$forecast(newdata,end)
                cur.max = apply(private$output$arr,c(1,3),max)
                cur.time = apply(private$output$arr,c(1,3),which.max)
            } else {
                cur.max = matrix(0,private$newdata$nrow,self$nsim)
                cur.time = matrix(1,private$newdata$nrow,self$nsim)
            }
            data <- SimulatedIncidenceMatrix$new(
                array(
                    (matrix(prev.max,nrow(cur.max),ncol(cur.max)) - cur.max > 0) * prev.time +
                        (matrix(prev.max,nrow(cur.max),ncol(cur.max)) - cur.max <= 0) * cur.time,
                    c(private$newdata$nrow,1,self$nsim),
                    list(
                        private$newdata$rnames,
                        'peak',
                        NULL
                    )
                )
            )
            data$dnames <- dnames
            
            return(
                IncidenceForecast$new(
                    data,
                    forecastTimes = TRUE
                )
            )
        },
        #' @method peakIncidence 
        #' @param newdata
        #' @param start Time Step relative to end of the data to start.  May be negative.  For example, -3 would mean 3 time steps before the last one in the data.  Note that the start is included in the range.
        #' @param end Time Step relative to the end of the data to stop.  May be negative.  For example 6 would mean 6 time steps after the last time step in the data.  Note that the end is included in the range.
        #' @return A SimulatedForecast containing the projected incidence at the time step containing the highest incidence.
        peakIncidence = function(newdata,start,end){
            if("peakIncidence" %in% private$.debug){browser()}
            private$newdata = IncidenceMatrix$new(newdata)
            dnames = list(
                private$newdata$rnames,
                paste('[',start,',',end,')'),
                NULL
            )
            if(start <= 0){
                private$newdata$tail(1-start)
                prev.max = apply(private$newdata$mat,1,max)
            } else {
                prev.max = 0
            }
            if(end > 0){
                self$forecast(newdata,end)
                cur.max = apply(private$output$arr,c(1,3),max)
            } else {
                cur.max = matrix(0,private$newdata$nrow,self$nsim)
            }
            data <- SimulatedIncidenceMatrix$new(
                array(
                    (matrix(prev.max,nrow(cur.max),ncol(cur.max)) - cur.max > 0) * prev.max +
                        (matrix(prev.max,nrow(cur.max),ncol(cur.max)) - cur.max <= 0) * cur.max,
                    c(private$newdata$nrow,1,self$nsim),
                    list(
                        private$newdata$rnames,
                        'peak',
                        NULL
                    )
                )
            )
            data$dnames <- dnames
            
            return(
                IncidenceForecast$new(
                    data,
                    forecastTimes = TRUE
                )
            )
        },
        totalIncidence = function(newdata,start,end){
            if("totalIncidence" %in% private$.debug){browser()}
            private$newdata = IncidenceMatrix$new(newdata)
            dnames = list(
                private$newdata$rnames,
                paste('[',start,',',end,')'),
                NULL
            )
            if(start <= 0){
                private$newdata$tail(1-start)
                prev.sum = apply(private$newdata$mat,1,sum,na.rm=T)
            } else {
                prev.sum = 0
            }
            if(end > 0){
                self$forecast(newdata,end)
                cur.sum = apply(private$output$arr,c(1,3),sum)
            } else {
                cur.sum = matrix(0,private$newdata$nrow,self$nsim)
            }
            data <- SimulatedIncidenceMatrix$new(
                array( prev.sum + cur.sum,
                    c(private$newdata$nrow,1,self$nsim),
                    list(
                        private$newdata$rnames,
                        'total',
                        NULL
                    )
                )
            )
            data$dnames <- dnames
            
            return(
                IncidenceForecast$new(
                    data,
                    forecastTimes = TRUE
                )
            )
            private$defaultAbstract()
        }
    ),
    active = list(
        nrow = function(value){
            if(missing(value)){
                return(private$.nrow)
            }
            stop("Do not write to the number of rows")
        },
        medians = function(value){
            if(missing(value)){
                return(private$.medians)
            }
            stop("Do not write to the medians")
        },
        nsim = function(value){
            if(missing(value)){
                return(1)
            }
            stop("This model doesn't even really make sense with this.")
        }
    )
)
