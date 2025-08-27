# Post GAMs creating a data frame for the DERIVATIVES of the norms of each LC type

library(mgcv) #load packages
library(ggplot2)
library(tibble)
library(dplyr)
library(MASS)

Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")
path.google <- ("~/Google Drive/My Drive/")
pathShare <- file.path(path.google, "../Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/")

source("~/Documents/GitHub/manuscript_NDVI_drought_monitoring/000_Calculate_GAMM_Derivatives.R")

######################
#loading in raw data
######################

raw.data <- read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/raw_and_harmonized_NDVI_all_LC.csv"))

newDF <- data.frame(yday=seq(1:365)) #create new data frame with column to represent day of year sequence

df <- data.frame()

######################
#loop through LC types
######################

for (LC in unique(raw.data$type)){
  datLC <- raw.data[raw.data$type==LC,]
  gamnorm <- gam(NDVIReprojected ~ s(yday, k=12, bs="cc"),data=datLC)
  derivs <- calc.derivs(model.gam=gamnorm, newdata = newDF, vars="yday")
  derivs$type <- LC
  
  df <- rbind(df,derivs)
}

write.csv(df, file.path(pathShare, "norms_derivatives.csv"),row.names=F) #save file

######################
