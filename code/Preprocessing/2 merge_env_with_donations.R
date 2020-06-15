library(lubridate)

# Load both datasets

env <- read.delim2("X:/202003 Ferritine en omgeving/Raw data/environment_by_donor.txt")
data <- readRDS("X:/202003 Ferritine en omgeving/donordata_with_temps.RDS")
env <- env[env$KeyID %in% data$DonorID, ]
colnames(env) <- c('DonorID', 'PM25_2018', 'PM10_2018', 'SES', 'PopDensity',
                   'O3', 'EC_2018', 'NO2_2018', 'PM25_2017', 'PM10_2017',
                   'EC_2017', 'NO2_2017')

# We want to merge the following variables from env into data, by DonorID:

# SES (2017)
# Population density (2018)
# O3 (2018)
# PM2.5 (2017 for donations in 2017-2018, 2018 for donations in 2019)
# PM10 (2017 for donations in 2017-2018, 2018 for donations in 2019)
# EC (2017 for donations in 2017-2018, 2018 for donations in 2019)
# NO2 (2017 for donations in 2017-2018, 2018 for donations in 2019)

data <- merge(data, env[, c('DonorID', 'SES', 'PopDensity', 'O3')], by='DonorID')

data1 <- merge(data[year(data$Date) %in% c(2017, 2018), ], 
               env[, c('DonorID', 'PM25_2017', 'PM10_2017', 'EC_2017', 'NO2_2017')],
               by='DonorID', all.x=TRUE)

data2 <- merge(data[year(data$Date) == 2019, ], 
               env[, c('DonorID', 'PM25_2018', 'PM10_2018', 'EC_2018', 'NO2_2018')],
               by='DonorID', all.x=TRUE)

colnames(data1)[25:28] <- colnames(data2)[25:28] <- c('PM25', 'PM10', 'EC', 'NO2')

data <- rbind(data1, data2)

saveRDS(data, "X:/202003 Ferritine en omgeving/donordata_with_temps_env.RDS")