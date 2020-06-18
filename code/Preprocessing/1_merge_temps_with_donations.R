require(plyr)
require(dplyr)
require(lubridate)
require(tidyr)
require(zoo)

# Load donation data

ddata <- readRDS("/data/vinkenoogm/FerritinDeterminants/donordata.RDS")

# Load AvgTemp data, which includes 7-day and 30-day rolling averages
# created by merge_avgtemps.R

AvgTemp <- readRDS("/data/vinkenoogm/FerritinDeterminants/AvgTemp.RDS")

# Transform Date variable to date format, add 1 day because we want to
# merge every donation with the temperature of the day before

AvgTemp$Date <- as.Date(substring(AvgTemp$Date, 3), format='%Y%m%d') + 1

# Merge AvgTemp with donation data

data <- merge(ddata, AvgTemp, by.x=c('Date', 'DonorID'), by.y=c('Date', 'KeyID'))

print("Done merging with average temperatures")

saveRDS(data, "/data/vinkenoogm/FerritinDeterminants/donordata_with_avg.RDS")

# Remove AvgTemp from memory, load MinTemp, follow same steps, and for MaxTemp too

rm(AvgTemp)

MinTemp <- readRDS("/data/vinkenoogm/FerritinDeterminants/MinTemp.RDS")
MinTemp$Date <- as.Date(substring(MinTemp$Date, 3), format='%Y%m%d') + 1
data <- merge(ddata, MinTemp, by.x=c('Date', 'DonorID'), by.y=c('Date', 'KeyID'))

print("Done merging with minimum temperatures")
saveRDS(data, "/data/vinkenoogm/FerritinDeterminants/donordata_with_avgmin.RDS")

rm(MinTemp)

MaxTemp <- readRDS("/data/vinkenoogm/FerritinDeterminants/MaxTemp.RDS")
MaxTemp$Date <- as.Date(substring(MaxTemp$Date, 3), format='%Y%m%d') + 1
data <- merge(ddata, MaxTemp, by.x=c('Date', 'DonorID'), by.y=c('Date', 'KeyID'))

print("Done merging with maximum temperatures")
saveRDS(data, "/data/vinkenoogm/FerritinDeterminants/donordata_with_temps.RDS")