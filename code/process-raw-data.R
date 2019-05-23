## process raw data into ForecastFramework

## devtools::install_github("HopkinsIDD/ForecastFramework")

library(ForecastFramework)
library(dplyr)

dat <- read.table("data/raw-rki-data-20190419.csv", sep="\t", skip=1, header=TRUE, fileEncoding="UTF-16LE", row.names = 1)

## replace NAs with 0s
na.idx <- which(is.na(dat), arr.ind=TRUE)
dat[na.idx] <- 0

## drop "Unknown" state
dat <- dat[-nrow(dat),]

### make season metadata
## this assumes that the "w.e." notation in the original file stands for week end
season_dat <- tibble(
    season.first.year = as.numeric(substr(colnames(dat), 2, 5)),
    season =  paste0(season.first.year, "/", season.first.year + 1),
    season.week = as.numeric(substr(colnames(dat), 12, 13)),
    week.end.month = as.numeric(substr(colnames(dat), 20, 21)),
    week.end.day = as.numeric(substr(colnames(dat), 23, 24)),
    ## logic for year is:
    ##    if it's an early month then it's second year in season
    ##    if it's a late month then it's first year in season
    ##    if it's a mid-year month then if it's a low season-week it's the first year and otherwise the second
    week.end.year = ifelse(week.end.month <= 5, season.first.year+1,
                           ifelse(week.end.month >= 8, season.first.year,
                                  ifelse(season.week < 10, season.first.year, season.first.year+1))),
    week.end.date = as.Date(paste(week.end.year, week.end.month, week.end.day, sep="-")),
    time.in.year = week.end.year + as.numeric(strftime(week.end.date, format="%j"))/as.numeric(strftime(paste0(week.end.year, "-12-31"), format="%j"))
    ) 

## rename columns of IncMat
colnames(dat) <- season_dat$week.end.date

## drop mid-summer week 53s
dat <- dat[,-which(season_dat$season.week==53)]
season_dat <- filter(season_dat, season.week != 53)

### put into ForecastFramework

## define training/testing seasons
training_seasons <- paste0(2001:2015, "/", 2002:2016)
testing_seasons <- paste0(2016:2018, "/", 2017:2019)

## make column data list
cdata <- list(
    'season.week' = season_dat$season.week, 
    'season' = season_dat$season, 
    'year' = season_dat$week.end.year,
    'week.end.date' = season_dat$week.end.date,
    'time.in.year' = season_dat$time.in.year)

## make metadata list
adjacency_mat <- read.csv("data/GER_states_adjacency.csv", row.names = 1)
humidity_dat <- read.csv("data/AbsHumidity.csv", row.names = 1)
initial <- as.matrix(read.csv("data/initials_new.csv"), header = TRUE)

metaData <- list(
    adjacency_mat = adjacency_mat,
    humidity_dat = humidity_dat,
    max.year.time = 365,
    initial = initial
)

## create objects
training_inc <- IncidenceMatrix$new(data = dat, colData=cdata, metaData=metaData)
training_inc$subset(cols = which(season_dat$season %in% training_seasons))

testing_inc <- IncidenceMatrix$new(data = dat, colData=cdata, metaData=metaData)

saveRDS(training_inc, file="data/training_data.rds")
saveRDS(testing_inc, file="data/testing_data.rds")


# plot(dat_year+dat_week/52, training_inc$mat[1,], type="l")
# for(i in 2:5) lines(dat_year+dat_week/52, tmp$mat[i,], col=i)


