# Post GAMs creating a data frame for the norms of each LC type

library(mgcv) #load packages
library(ggplot2)
library(tibble)
library(dplyr)
library(MASS)

Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")
path.google <- ("~/Google Drive/My Drive/")
pathShare <- file.path(path.google, "../Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/")

source("~/Documents/GitHub/manuscript_NDVI_drought_monitoring/0_Calculate_GAMM_Posteriors.R")

######################
#loading in and formatting raw data from 01_raw_data.R
######################

raw.data <- read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/raw_and_reprojected_NDVI_all_LC.csv"))

newDF <- data.frame(yday=seq(1:365)) #create new data frame with column to represent day of year sequence

######################
#loop through each LC
######################
df <- data.frame()

for (LC in unique(raw.data$type)){
  datLC <- raw.data[raw.data$type==LC,]
  
  gam_norm <- gam(NDVIReprojected ~ s(yday, k=12, bs="cc"), data=datLC) #cyclic cubic spline for norm
  norm_post <- post.distns(model.gam=gam_norm, newdata=newDF, vars="yday")
  norm_post$type <- LC
  df <- rbind(df,norm_post)
}

write.csv(df, file.path(pathShare, "norms_all_LC_types.csv"), row.names=F)

######################