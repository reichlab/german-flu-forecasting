## process raw data into ForecastFramework

## devtools::install_github("HopkinsIDD/ForecastFramework")

library(ForecastFramework)
library(dplyr)

dat <- read.table("data/raw-rki-data.csv", sep="\t", skip=1, header=TRUE, fileEncoding="UTF-16LE", row.names = 1)

## replace NAs with 0s
na.idx <- which(is.na(dat), arr.ind=TRUE)
dat[na.idx] <- 0

## drop "Unknown" state
dat <- dat[-nrow(dat),]

## make season metadata
season_dat <- tibble(
    week = as.numeric(substr(colnames(dat), 8, 9)),
    year = as.numeric(substr(colnames(dat), 2, 5)),
    season =  ifelse(
        dat_week > 26,
        paste0(dat_year, "/", dat_year+1),
        paste0(dat_year-1, "/", dat_year))) %>%
    group_by(season) %>%
    mutate(season_week = row_number()) %>%
    ungroup()

## drop mid-summer week 53s
dat <- dat[,-which(season_dat$season_week==53)]
season_dat <- filter(season_dat, season_week != 53)

## put into ForecastFramework
training_seasons <- paste0(2001:2015, "/", 2002:2016)
testing_seasons <- paste0(2016:2018, "/", 2017:2019)

cdata <- list(
    'season.week' = season_dat$season_week, 
    'season' = season_dat$season, 
    'year' = season_dat$year,
    'week' = season_dat$week)
training_inc <- IncidenceMatrix$new(data = dat, colData=cdata)
training_inc$subset(cols = which(season_dat$season %in% training_seasons))

testing_inc <- IncidenceMatrix$new(data = dat, colData=cdata)
testing_inc$subset(cols = which(season_dat$season %in% testing_seasons))

saveRDS(training_inc, file="data/training_data.rds")
saveRDS(testing_inc, file="data/testing_data.rds")


# plot(dat_year+dat_week/52, training_inc$mat[1,], type="l")
# for(i in 2:5) lines(dat_year+dat_week/52, tmp$mat[i,], col=i)


