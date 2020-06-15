require(plyr)
require(dplyr)
require(lubridate)
require(tidyr)
require(zoo)


# Load MaxTemp17/18/19, cbind on KeyID, remove separate files

MaxTemp17 <- read.delim2("/data/vinkenoogm/FerritinDeterminants/maximumTemperature17.txt")
MaxTemp18 <- read.delim2("/data/vinkenoogm/FerritinDeterminants/maximumTemperature18.txt")
MaxTemp19 <- read.delim2("/data/vinkenoogm/FerritinDeterminants/maximumTemperature19.txt")
MaxTemp <- merge(MaxTemp17, MaxTemp18, by='KeyID')
MaxTemp <- merge(MaxTemp, MaxTemp19, by='KeyID')
rm(MaxTemp17, MaxTemp18, MaxTemp19)

# -9999 is used as NA. Replace this and any outliers. -20 is a safe cutoff for Dutch temps. 
# KeyIDs are never negative so they are safe 

MaxTemp[MaxTemp < -20] <- NA

# Pivot to get MaxTemp of the same day

z0 <- MaxTemp %>% pivot_longer(-KeyID, names_to='Date', values_to='MaxTemp')

# Rollmean, then pivot to get average MaxTemp of the past 7 days

y <- as.data.frame(apply(MaxTemp[, -1], MARGIN=1, FUN = rollmean, k=7, fill=NA, align="right", na.rm=TRUE))
colnames(y) <- MaxTemp$KeyID
rownames(y) <- colnames(MaxTemp)[-1]
z1 <- y %>% tibble::rownames_to_column(var='Date') %>% pivot_longer(-Date, names_to='KeyID', values_to='MaxTemp7')

# Rollmean, then pivot to get average MaxTemp of the past 30 days

y <- as.data.frame(apply(MaxTemp[, -1], MARGIN=1, FUN = rollmean, k=30, fill=NA, align="right", na.rm=TRUE))
colnames(y) <- MaxTemp$KeyID
rownames(y) <- colnames(MaxTemp)[-1]
z2 <- y %>% tibble::rownames_to_column(var='Date') %>% pivot_longer(-Date, names_to='KeyID', values_to='MaxTemp30')

# Merge past three steps to get a dataframe with MaxTemp of 1 day, 7 day avg and 30 day avg, 
# remove separate files

MaxTemp <- merge(z0, z1)
MaxTemp <- merge(MaxTemp, z2)
rm(y, z0, z1, z2)

saveRDS(MaxTemp, "/data/vinkenoogm/FerritinDeterminants/MaxTemp.RDS")