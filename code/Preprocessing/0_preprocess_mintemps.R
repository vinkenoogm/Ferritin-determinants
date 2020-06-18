require(plyr)
require(dplyr)
require(lubridate)
require(tidyr)
require(zoo)


# Load MinTemp17/18/19, cbind on KeyID, remove separate files

MinTemp17 <- read.delim2("/data/vinkenoogm/FerritinDeterminants/minimumTemperature17.txt")
MinTemp18 <- read.delim2("/data/vinkenoogm/FerritinDeterminants/minimumTemperature18.txt")
MinTemp19 <- read.delim2("/data/vinkenoogm/FerritinDeterminants/minimumTemperature19.txt")
MinTemp <- merge(MinTemp17, MinTemp18, by='KeyID')
MinTemp <- merge(MinTemp, MinTemp19, by='KeyID')
rm(MinTemp17, MinTemp18, MinTemp19)

# -9999 is used as NA. Replace this and any outliers. -20 is a safe cutoff for Dutch temps. 
# KeyIDs are never negative so they are safe 

MinTemp[MinTemp < -20] <- NA

# Pivot to get MinTemp of the same day

z0 <- MinTemp %>% pivot_longer(-KeyID, names_to='Date', values_to='MinTemp')

# Rollmean, then pivot to get average MinTemp of the past 7 days

y <- as.data.frame(apply(MinTemp[, -1], MARGIN=1, FUN = rollmean, k=7, fill=NA, align="right", na.rm=TRUE))
colnames(y) <- MinTemp$KeyID
rownames(y) <- colnames(MinTemp)[-1]
z1 <- y %>% tibble::rownames_to_column(var='Date') %>% pivot_longer(-Date, names_to='KeyID', values_to='MinTemp7')

# Rollmean, then pivot to get average MinTemp of the past 30 days

y <- as.data.frame(apply(MinTemp[, -1], MARGIN=1, FUN = rollmean, k=30, fill=NA, align="right", na.rm=TRUE))
colnames(y) <- MinTemp$KeyID
rownames(y) <- colnames(MinTemp)[-1]
z2 <- y %>% tibble::rownames_to_column(var='Date') %>% pivot_longer(-Date, names_to='KeyID', values_to='MinTemp30')

# Merge past three steps to get a dataframe with MinTemp of 1 day, 7 day avg and 30 day avg, 
# remove separate files

MinTemp <- merge(z0, z1)
MinTemp <- merge(MinTemp, z2)
rm(y, z0, z1, z2)

saveRDS(MinTemp, "/data/vinkenoogm/FerritinDeterminants/MinTemp.RDS")