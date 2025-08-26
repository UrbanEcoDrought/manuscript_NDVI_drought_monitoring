# formatting raw NDVI data separately by LC type and saving into dataframe before fitting monitoring GAMs

library(mgcv) #load packages
library(ggplot2)
library(tibble)
library(dplyr)
library(MASS)

Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")
path.google <- ("~/Google Drive/My Drive/")
pathShare <- file.path(path.google, "../Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/")

# load & format data ------------------------------------------------------

ndvi.latest <- read.csv(file.path(google.drive, "data/UrbanEcoDrought_NDVI_LocalExtract/NDVIall_latest.csv"))
ndvi.latest$date <- as.Date(ndvi.latest$date)
ndvi.latest$type <- as.factor(ndvi.latest$type)
ndvi.latest$mission <- as.factor(ndvi.latest$mission)
summary(ndvi.latest)

#for this project we're using 2001-2024
ndvi.latest <- subset(ndvi.latest, date >= as.Date('2001-01-01') & date <= as.Date('2024-12-31'))

# run gam for each LC and reproject ---------------------------------------

df <- data.frame()

for (LC in unique(ndvi.latest$type)){
  datLC <- ndvi.latest[ndvi.latest$type==LC,]
  
  gamLC <- gam(NDVI ~ s(yday, k=12, by=mission) + mission-1, data=datLC)
  datLC$NDVIMissionPred <- predict(gamLC, newdata=datLC)
  datLC$MissionResid <- datLC$NDVI - datLC$NDVIMissionPred
  
  #reproject
  datLCDupe <- datLC
  datLCDupe$mission <- "landsat 8"
  
  datLC$ReprojPred <- predict(gamLC, newdata=datLCDupe)
  datLC$NDVIReprojected <- datLC$MissionResid + datLC$ReprojPred
  
  df <- rbind(df, datLC)
}

# save --------------------------------------------------------------------

write.csv(df, file.path(pathShare, "raw_and_harmonized_NDVI_all_LC.csv"), row.names=F)

######################