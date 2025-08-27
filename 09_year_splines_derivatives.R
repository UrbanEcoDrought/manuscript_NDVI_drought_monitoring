#creating separate data frame for derivatives based on gams of inividual years

library(mgcv) #load packages
library(ggplot2)
library(MASS)
library(lubridate)

Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")
path.google <- ("~/Google Drive/My Drive/")
pathShare <- file.path(path.google, "../Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/")

source("~/Documents/GitHub/manuscript_NDVI_drought_monitoring/000_Calculate_GAMM_Derivatives.R")

######################
#loading in raw data
######################

raw.data <- read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/raw_and_harmonized_NDVI_all_LC.csv"))

newDF <- data.frame(yday=seq(1:365)) #create new data frame to predict over

df <- data.frame()

######################
#loop through LC types and years
######################

for (LC in unique(raw.data$type)){
  datLC <- raw.data[raw.data$type==LC,]
  
  for (yr in unique(datLC$year)){
    datyr <- datLC[datLC$year==yr,]
    
    prev_dec <- datLC[datLC$yday > (365-31) & datLC$year==yr-1,]
    prev_dec <- prev_dec %>% mutate(year=yr, yday = yday-365-1)
    
    next_jan <- datLC[datLC$yday <= 31 & datLC$year==yr+1,]
    next_jan <- next_jan %>% mutate(year=yr, yday = yday +365)
    
    datyr <- bind_rows(datyr, prev_dec, next_jan)
    
    if(yr==2024 | yr==2001){
      gamyr <- gam(NDVIReprojected ~ s(yday, k=13), data=datyr)
    }else{
      gamyr <- gam(NDVIReprojected ~ s(yday, k=14), data=datyr)
    }
    
    #gampred <- predict(gamyr, newdata=newDF)
    derivs <- calc.derivs(model.gam=gamyr, newdata=newDF, vars="yday")
    derivs$type <- LC
    derivs$year <- yr
    #post$NDVIpred <- gampred
    
    df <- rbind(df,derivs)
  }
}

write.csv(df, file.path(pathShare, "year_splines_derivatives.csv"), row.names=F) #save file

######################
