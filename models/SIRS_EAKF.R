library(ForecastFramework)
library(R6)
library(forecast)
library(lubridate)

source_github <- function(u) {
  library(RCurl)
  # read script lines from website and evaluate
  script <- getURL(u, ssl.verifypeer = FALSE)
  eval(parse(text = script),envir=.GlobalEnv)
}

source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/ContestModel.R')

# SIRS function
SIRS <- function(x,ts,dt,tmstep,N,AH,seed,discrete) {
  library(matrixStats)
  library(abind)
  # x=[S,I,total incidence,R0max,R0min,L,D]
  if (discrete==0) {
    AH=c(AH,AH)
    num_ens=dim(x)[2]
    BT1=matrix(0,length(AH),num_ens)
    BETA=matrix(0,length(AH),num_ens)
    for (i in 1:num_ens) {
      # b=log(x[4,i]-x[5,i])
      if ((x[4,i]-x[5,i])>=0) {
        b=log(x[4,i]-x[5,i])
      }
      else {
        b=complex(real=log(-(x[4,i]-x[5,i])),imaginary = pi)
      }
      a=-180
      BT1[,i]=exp(a*AH+b)+x[5,i]
      BETA[,i]=BT1[,i]/x[7,i]
    }
    # Sr=array(0,c(3,num_ens,tmstep+1))
    # Incidence=matrix(0,(tmstep+1),num_ens)
    Sr=array(0,c(3,num_ens,abs(tmstep)+1))
    Incidence=matrix(0,(abs(tmstep)+1),num_ens)
    Sr[,,1]=x[1:3,]
    L=x[6,]
    D=x[7,]
    # start integration
    tcnt=0
    for (t in seq(ts+dt,ts+tmstep,by = dt)) {
      tcnt=tcnt+1
      Eimmloss=dt*((N*rep(1,num_ens)-Sr[1,,tcnt]-Sr[2,,tcnt])/L)
      tempind=Mod(dt*(BETA[t,]*Sr[1,,tcnt]*Sr[2,,tcnt]/N))>Mod(Sr[1,,tcnt])
      Einf=dt*(BETA[t,]*Sr[1,,tcnt]*Sr[2,,tcnt]/N)
      Einf[tempind]=Sr[1,,tcnt][tempind]
      tempind2=Mod(dt*(Sr[2,,tcnt]/D))>Mod(Sr[2,,tcnt])
      Erecov=dt*(Sr[2,,tcnt]/D)
      Erecov[tempind2]=Sr[2,,tcnt][tempind2]
      Eimmloss[Mod(Eimmloss)<0]=0
      Einf[Mod(Einf)<0]=0
      Erecov[Mod(Erecov)<0]=0
      sk1=Eimmloss-Einf
      ik1=Einf-Erecov
      ik1i=Einf
      Ts1=Sr[1,,tcnt]+round(sk1/2)
      Ti1=Sr[2,,tcnt]+round(ik1/2)
      Eimmloss=dt*((N*rep(1,num_ens)-Ts1-Ti1)/L)
      tempind=Mod(dt*(BETA[t,]*Ts1*Ti1/N))>Mod(Ts1)
      Einf=dt*(BETA[t,]*Ts1*Ti1/N)
      Einf[tempind]=Ts1[tempind]
      tempind2=Mod(dt*(Ti1/D))>Mod(Ti1)
      Erecov=dt*(Ti1/D)
      Erecov[tempind2]=Ti1[tempind2]
      Eimmloss[Mod(Eimmloss)<0]=0
      Einf[Mod(Einf)<0]=0
      Erecov[Mod(Erecov)<0]=0
      sk2=Eimmloss-Einf
      ik2=Einf-Erecov
      ik2i=Einf
      Ts2=Sr[1,,tcnt]+round(sk2/2)
      Ti2=Sr[2,,tcnt]+round(ik2/2)
      Eimmloss=dt*((N*rep(1,num_ens)-Ts2-Ti2)/L)
      tempind=Mod(dt*(BETA[t,]*Ts2*Ti2/N))>Mod(Ts2)
      Einf=dt*(BETA[t,]*Ts2*Ti2/N)
      Einf[tempind]=Ts2[tempind]
      tempind2=Mod(dt*(Ti2/D))>Mod(Ti2)
      Erecov=dt*(Ti2/D)
      Erecov[tempind2]=Ti2[tempind2]
      Eimmloss[Mod(Eimmloss)<0]=0
      Einf[Mod(Einf)<0]=0
      Erecov[Mod(Erecov)<0]=0
      sk3=Eimmloss-Einf
      ik3=Einf-Erecov
      ik3i=Einf
      Ts3=Sr[1,,tcnt]+round(sk3)
      Ti3=Sr[2,,tcnt]+round(ik3)
      Eimmloss=dt*((N*rep(1,num_ens)-Ts3-Ti3)/L)
      tempind=Mod(dt*(BETA[t,]*Ts3*Ti3/N))>Mod(Ts3)
      Einf=dt*(BETA[t,]*Ts3*Ti3/N)
      Einf[tempind]=Ts3[tempind]
      tempind2=Mod(dt*(Ti3/D))>Mod(Ti3)
      Erecov=dt*(Ti3/D)
      Erecov[tempind2]=Ti3[tempind2]
      Eimmloss[Mod(Eimmloss)<0]=0
      Einf[Mod(Einf)<0]=0
      Erecov[Mod(Erecov)<0]=0
      sk4=Eimmloss-Einf
      ik4=Einf-Erecov
      ik4i=Einf
      Sr[1,,tcnt+1]=Sr[1,,tcnt]+round(sk1/6+sk2/3+sk3/3+sk4/6)-seed
      Sr[2,,tcnt+1]=Sr[2,,tcnt]+round(ik1/6+ik2/3+ik3/3+ik4/6)+seed
      Incidence[tcnt+1,]=round(ik1i/6+ik2i/3+ik3i/3+ik4i/6)+seed
    }
    x[1,]=Sr[1,,tcnt+1]
    x[2,]=Sr[2,,tcnt+1]
    x[3,]=colSums(Incidence)/(N/100000)
  } 
  
  if (discrete==1) {
    AH=c(AH,AH)
    num_ens=dim(x)[2]
    BT1=matrix(0, length(AH),num_ens)
    BETA=matrix(0,length(AH),num_ens)
    for (i in 1:num_ens) {
      # b=log(x[4,i]-x[5,i])
      if ((x[4,i]-x[5,i])>=0) {
        b=log(x[4,i]-x[5,i])
      }
      else {
        b=complex(real=log(-(x[4,i]-x[5,i])),imaginary = pi)
      }
      a=-180
      BT1[,i]=exp(a*AH+b)+x[5,i]
      BETA[,i]=BT1[,i]/x[7,i]
    }
    Sr=array(0,c(3,num_ens,abs(tmstep)+1))
    Incidence=matrix(0,abs(tmstep)+1,num_ens)
    Sr[,,1]=x[1:3,]
    L=x[6,]
    D=x[7,]
    # start integration
    tcnt=0
    for (t in seq(ts+dt,ts+tmstep,by = dt)) {
      tcnt=tcnt+1
      Eimmloss=dt*((N*rep(1,num_ens)-Sr[1,,tcnt]-Sr[2,,tcnt])/L)
      tempind=Mod(dt*(BETA[t,]*Sr[1,,tcnt]*Sr[2,,tcnt]/N))>Mod(Sr[1,,tcnt])
      Einf=dt*(BETA[t,]*Sr[1,,tcnt]*Sr[2,,tcnt]/N)
      Einf[tempind]=Sr[1,,tcnt][tempind]
      tempind2=Mod(dt*(Sr[2,,tcnt]/D))>Mod(Sr[2,,tcnt])
      Erecov=dt*(Sr[2,,tcnt]/D)
      Erecov[tempind2]=Sr[2,,tcnt][tempind2]
      Eimmloss[Mod(Eimmloss)<0]=0
      Einf[Mod(Einf)<0]=0
      Erecov[Mod(Erecov)<0]=0
      l1=rbind(Eimmloss,Einf,Erecov)
      l <- rpois(length(l1), l1)
      dim(l) <- dim(l1)
      sk1=l[1,]-l[2,]
      ik1=l[2,]-l[3,]
      ik1i=l[2,]
      Ts1=Sr[1,,tcnt]+round(sk1/2)
      Ti1=Sr[2,,tcnt]+round(ik1/2)
      Eimmloss=dt*((N*rep(1,num_ens)-Ts1-Ti1)/L)
      tempind=Mod(dt*(BETA[t,]*Ts1*Ti1/N))>Mod(Ts1)
      Einf=dt*(BETA[t,]*Ts1*Ti1/N)
      Einf[tempind]=Ts1[tempind]
      tempind2=Mod(dt*(Ti1/D))>Mod(Ti1)
      Erecov=dt*(Ti1/D)
      Erecov[tempind2]=Ti1[tempind2]
      Eimmloss[Mod(Eimmloss)<0]=0
      Einf[Mod(Einf)<0]=0
      Erecov[Mod(Erecov)<0]=0
      l1=rbind(Eimmloss,Einf,Erecov)
      l <- rpois(length(l1), l1)
      dim(l) <- dim(l1)
      sk2=l[1,]-l[2,]
      ik2=l[2,]-l[3,]
      ik2i=l[2,]
      Ts2=Sr[1,,tcnt]+round(sk2/2)
      Ti2=Sr[2,,tcnt]+round(ik2/2)
      Eimmloss=dt*((N*rep(1,num_ens)-Ts2-Ti2)/L)
      tempind=Mod(dt*(BETA[t,]*Ts2*Ti2/N))>Mod(Ts2)
      Einf=dt*(BETA[t,]*Ts2*Ti2/N)
      Einf[tempind]=Ts2[tempind]
      tempind2=Mod(dt*(Ti2/D))>Mod(Ti2)
      Erecov=dt*(Ti2/D)
      Erecov[tempind2]=Ti2[tempind2]
      Eimmloss[Mod(Eimmloss)<0]=0
      Einf[Mod(Einf)<0]=0
      Erecov[Mod(Erecov)<0]=0
      l1=rbind(Eimmloss,Einf,Erecov)
      l <- rpois(length(l1), l1)
      dim(l) <- dim(l1)
      sk3=l[1,]-l[2,]
      ik3=l[2,]-l[3,]
      ik3i=l[2,]
      Ts3=Sr[1,,tcnt]+round(sk3)
      Ti3=Sr[2,,tcnt]+round(ik3)
      Eimmloss=dt*((N*rep(1,num_ens)-Ts3-Ti3)/L)
      tempind=Mod(dt*(BETA[t,]*Ts3*Ti3/N))>Mod(Ts3)
      Einf=dt*(BETA[t,]*Ts3*Ti3/N)
      Einf[tempind]=Ts3[tempind]
      tempind2=Mod(dt*(Ti3/D))>Mod(Ti3)
      Erecov=dt*(Ti3/D)
      Erecov[tempind2]=Ti3[tempind2]
      Eimmloss[Mod(Eimmloss)<0]=0
      Einf[Mod(Einf)<0]=0
      Erecov[Mod(Erecov)<0]=0
      l1=rbind(Eimmloss,Einf,Erecov)
      l <- rpois(length(l1), l1)
      dim(l) <- dim(l1)
      sk4=l[1,]-l[2,]
      ik4=l[2,]-l[3,]
      ik4i=l[2,]
      travel=rpois(1,seed)
      Sr[1,,tcnt+1]=Sr[1,,tcnt]+round(sk1/6+sk2/3+sk3/3+sk4/6)-travel
      Sr[2,,tcnt+1]=Sr[2,,tcnt]+round(ik1/6+ik2/3+ik3/3+ik4/6)+travel
      Incidence[tcnt+1,]=round(ik1i/6+ik2i/3+ik3i/3+ik4i/6)+travel
    }
    
    x[1,]=Sr[1,,tcnt+1]
    x[2,]=Sr[2,,tcnt+1]
    x[3,]=colSums(Incidence)/(N/100000)
    
  }
  
  return (x)
}

# checkDA function
checkDA = function(x) {
  N=500000
  ug=max(x[1,]) # Corrects if S>N
  if (ug>N) {
    x[1,x[1,]>N]=N-1
  }
  ug=max(x[2,]) # Corrects if I>N
  if (ug>N) {
    x[2,x[2,]>N]=median(x[2,])
  }
  ug=min(min(x)) # Corrects if any state or parameter nudges negative
  if (ug<=0) {
    for (i in 1:7) {
      x[i,x[i,]<0]=mean(x[i,])
    }
  }
  ug=min(x[6,]) # Corrects if L < 200 days
  if (ug<200) {
    x[6,x[6,]<200]=median(x[6,])
  }
  ug=min(x[7,]) # Corrects if D < .5
  if (ug<=.5) {
    x[7,x[7,]<0.5]=median(x[7,])
  }
  ug=min(x[4,]-x[5,]) # Corrects if R0mx <= R0mn
  if (ug<=0) {
    x[4,x[4,]<x[5,]]=x[5,x[4,]<x[5,]]+0.01
  }
  return (x)
}

# load data
training_data <- readRDS("data/training_data.rds")
initial <- as.matrix(read.csv("data/initials_new.csv"), header = TRUE)
training_data$metaData$initial <- initial

SIRS_EAKF <- R6Class(
  inherit = ContestModel,
  
  private = list(
    .data = NULL, # ILIplus
    .num_ens = integer(0), # number of ensembles
    .num_times = integer(0), # number of weeks in each season
    .N = integer(0), # total population
    .scale = integer(0), # scale parameter of ILIplus
    .seed = integer(0), # seed in SIRS model
    .discrete = integer(0), # discrete (0/1) in SIRS model
    .dt = integer(0), # time unit
    .lambda = integer(0), # inflation factor
    .tmstep = integer(0), # number of runs, far enough so model spreads
    
    .obs = c(), # number of obsevations
    .ts = NULL, # start date of that year
    .AH = c(), # AbsHumidity
    
    .fitData = NULL # fitted state variables at the end of num_times
  ),
  
  public = list(
    fit = function(data) {
      if ("fit" %in% private$.debug) {
        browser()
      } # include for debugging
      
      # load data
      private$.data <- data
      AbsHumidity <- data$metaData$humidity_dat
      
      x_fit <- replicate(nrow(data$mat),matrix(0,7,100))
      
      for (city_id in 1:nrow(data$mat)) {
        
        # load ILI+, prepare observations
        # city_id <- which(data$rnames == private$.city)
        ILIp <- private$.data$subset(rows = city_id, mutate = FALSE)
        ILIp <- as.numeric(ILIp$mat)
        private$.obs <- ILIp * private$.scale 
        
        # load AbsHumidity
        AH <- AbsHumidity[, 1]
        AH <- rep(AH, ceiling(length(ILIp)*7/365))
        private$.AH <- AH
        
       # year <- substr(data$cnames, 2, 5)
       # month <- substr(data$cnames, 20, 21)
       # day <- substr(data$cnames, 23, 24)
       # date <- paste0(year,"/",month,"/",day)
        private$.ts <- yday(data$cnames[1]) # start date number in the year
        
        # mapping operator
        HH <- diag(7)
        H <- HH[3, ]
        
        x <- data$metaData$initial

        x <- SIRS(
          x, private$.ts, private$.dt, private$.tmstep,
          private$.N, private$.AH, private$.seed, private$.discrete
        )
        
        ### Begin looping through observations
        private$.num_times <- length(ILIp)
        xprior <- replicate(private$.num_times, matrix(NaN, 7, private$.num_ens))
        xpost <- xprior
        for (tt in 1:private$.num_times) {
          # Get the variance of the ensemble
          ave <- private$.obs[max(1, tt - 1)]
          ave <- ave + private$.obs[max(1, tt - 2)]
          ave <- ave + private$.obs[max(1, tt - 3)]
          ave <- ave / 3
          obs_var <- 10^5 + ave^2 / 5
          # inflation of x before assimilation
          x <- as.matrix(apply(x, 1, mean)) %*% rep(1, private$.num_ens) +
            private$.lambda * (x - as.matrix(apply(x, 1, mean)) %*% rep(1, private$.num_ens))
          prior_var <- var(as.vector(H %*% x))
          post_var <- prior_var * obs_var / (prior_var + obs_var)
          if (prior_var == 0) {
            post_var <- 0
          }
          prior_mean <- mean(H %*% x)
          post_mean <- post_var * (prior_mean / prior_var + private$.obs[tt] / obs_var)
          # Compute alpha and adjust distribution to conform to posterior moments
          alpha <- (obs_var / (obs_var + prior_var))^0.5
          dy <- post_mean + alpha * ((H %*% x) - prior_mean) - H %*% x
          #  Loop over each state variable
          rr <- matrix(0, 1, dim(x)[1])
          for (j in 1:dim(x)[1]) {
            A <- cov(x[j, ], as.vector(H %*% x))
            rr[j] <- A / prior_var
          }
          dx <- t(rr) %*% dy
          #  Get the new ensemble and save prior and posterior
          xprior[, , tt] <- x
          xnew <- x + dx
          #  Corrections to DA produced aphysicalities
          xnew <- checkDA(xnew)
          xpost[, , tt] <- xnew
          #  Integrate forward one time step
          tcurrent <- private$.ts + private$.tmstep * tt
          x <- SIRS(
            xnew, tcurrent, private$.dt, private$.tmstep,
            private$.N, private$.AH, private$.seed, private$.discrete
          )
          # EAKF update is finished
        }
        x_fit[,,city_id] <- x
      }
      private$.fitData <- x_fit
    },
    
    forecast = function(newdata=private$.data,steps) {
      x_fit <- private$.fitData
      x_forecast <- replicate(nrow(private$.data$mat),matrix(0,steps,private$.num_ens+1))
      
      for (city_id in 1:nrow(private$.data$mat)) {
        
        forecasts <- matrix(0,steps,private$.num_ens+1)
        
        x <- x_fit[,,city_id]
        x[2,]<-x[3,]
        #x[2,]<-private$.obs[length(private$.obs)]*rep(1,private$.num_ens)
        for (tt in 1:steps) {
          #  Integrate forward
          tcurrent <- private$.ts + private$.tmstep * (tt + private$.num_times)
          x <- SIRS(
            x, tcurrent, private$.dt, private$.tmstep,
            private$.N, private$.AH, private$.seed, private$.discrete
          )
          forecasts[tt,1:private$.num_ens] <- x[2,]
          forecasts[tt,private$.num_ens+1] <- rowMeans(x)[2]
        }
        x_forecast[,,city_id] <- forecasts/private$.scale
      }
      x_forecast <- aperm(x_forecast,c(3,1,2)) # transpose 3d array
      rownames(x_forecast)<- private$.data$rnames
      private$output <-  SimulatedIncidenceMatrix$new(x_forecast)
      return(IncidenceForecast$new(private$output,forecastTimes = rep(TRUE, steps)))
    },
    initialize = function(num_ens = 100, N = 500000, lambda = 1.00, dt = 1, seed = 0.001,
                          scale = 30, discrete = 0, tmstep = 7) {
      private$.num_ens <- num_ens # number of ensembles
      private$.N <- N # total population
      private$.lambda <- lambda # inflation factor
      private$.dt <- dt # time unit
      private$.seed <- seed # seed in SIRS model
      private$.scale <- scale # scale parameter of ILIplus
      private$.discrete <- discrete # discrete (0/1) in SIRS model
      private$.tmstep <- tmstep # number of runs, far enough so model spreads
    },
    fit_once = TRUE
  )
)




