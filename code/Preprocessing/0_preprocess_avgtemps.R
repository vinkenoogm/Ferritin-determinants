require(plyr)
require(dplyr)
require(lubridate)
require(tidyr)
require(zoo)


# Load AvgTemp17/18/19, cbind on KeyID, remove separate files

AvgTemp17 <- read.delim2("/data/vinkenoogm/FerritinDeterminants/AverageTemperature17.txt")
AvgTemp18 <- read.delim2("/data/vinkenoogm/FerritinDeterminants/AverageTemperature18.txt")
AvgTemp19 <- read.delim2("/data/vinkenoogm/FerritinDeterminants/AverageTemperature19.txt")
AvgTemp <- merge(AvgTemp17, AvgTemp18, by='KeyID')
AvgTemp <- merge(AvgTemp, AvgTemp19, by='KeyID')
rm(AvgTemp17, AvgTemp18, AvgTemp19)

# -9999 is used as NA. Replace this and any outliers. -20 is a safe cutoff for Dutch temps. 
# KeyIDs are never negative so they are safe 

AvgTemp[AvgTemp < -20] <- NA

# Pivot to get AvgTemp of the same day

z0 <- AvgTemp %>% pivot_longer(-KeyID, names_to='Date', values_to='AvgTemp')

# Rollmean, then pivot to get average AvgTemp of the past 7 days

y <- as.data.frame(apply(AvgTemp[, -1], MARGIN=1, FUN = rollmean, k=7, fill=NA, align="right", na.rm=TRUE))
colnames(y) <- AvgTemp$KeyID
rownames(y) <- colnames(AvgTemp)[-1]
z1 <- y %>% tibble::rownames_to_column(var='Date') %>% pivot_longer(-Date, names_to='KeyID', values_to='AvgTemp7')

# Rollmean, then pivot to get average AvgTemp of the past 30 days

y <- as.data.frame(apply(AvgTemp[, -1], MARGIN=1, FUN = rollmean, k=30, fill=NA, align="right", na.rm=TRUE))
colnames(y) <- AvgTemp$KeyID
rownames(y) <- colnames(AvgTemp)[-1]
z2 <- y %>% tibble::rownames_to_column(var='Date') %>% pivot_longer(-Date, names_to='KeyID', values_to='AvgTemp30')

# Merge past three steps to get a dataframe with AvgTemp of 1 day, 7 day avg and 30 day avg, 
# remove separate files

AvgTemp <- merge(z0, z1)
AvgTemp <- merge(AvgTemp, z2)
rm(y, z0, z1, z2)

saveRDS(AvgTemp, "/data/vinkenoogm/FerritinDeterminants/AvgTemp.RDS")