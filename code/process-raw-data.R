## process raw data into ForecastFramework

## devtools::install_github("HopkinsIDD/ForecastFramework")

library(ForecastFramework)

dat <- read.table("data/raw-rki-data.csv", sep="\t", skip=1, header=TRUE, fileEncoding="UTF-16LE")

