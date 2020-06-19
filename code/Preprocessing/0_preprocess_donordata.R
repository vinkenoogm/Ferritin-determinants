library(dplyr)

ddata_raw <- read.delim("X:/202003 Ferritine en omgeving/Raw data/donations_201710_201912.dat")
ddata_raw <- ddata_raw %>% mutate_at(vars(Geboortedatum, Donatiedatum, IndexDonatie), as.Date, format='%m/%d/%Y')

ddata <- ddata_raw[ddata_raw$IndexDonatie == ddata_raw$Donatiedatum, 
                   c('KeyID', 'Geboortedatum', 'Geslacht', 'Donatiedatum', 'Hb', 'Ferritine', 'donatiesoort', 
                     'DagenLaatsteDonatie', 'AantalBeforeIndex', 'Bloedgroep', 'bloedvolume',
                     'BMI', 'Duffy_proxy')]
colnames(ddata) <- c('DonorID', 'DoB', 'Sex', 'Date', 'Hb', 'Ferritin', 'DonType', 'DaysSincePrev', 'NumBefore', 
                     'BloodType', 'BloodVolume', 'BMI', 'Duffy')
ddata$Sex <- as.factor(ddata$Sex)
ddata$DonType <- factor(ddata$DonType, levels=c(0,1,2,3), labels=c('V', 'H', 'N', 'Other'))
ddata$Duffy <- factor(ddata$Duffy, levels=c(0,1), labels=c('No', 'Yes'))
ddata$Age <- as.numeric(ddata$Date - ddata$DoB) / 365.25
ddata <- ddata[c('DonorID', 'Sex', 'Age', 'Date', 'Ferritin', 'DonType', 'DaysSincePrev', 'NumBefore', 
                 'BloodType', 'BloodVolume', 'BMI', 'Duffy')]

ddata <- ddata[ddata$DonType %in% c('N', 'V'), ]
ddata$DonType <- droplevels(ddata$DonType)
ddata$DonorID <- as.factor(ddata$DonorID)

ddata <- subset(ddata, !(DonType=='N' & !is.na(DaysSincePrev)))

saveRDS(ddata, "X:/202003 Ferritine en omgeving/donordata")