#code for finding growing season of each LC type based on normal

library(spatialEco)
library(ggplot2)
library(tidyr)
library(dplyr)

Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")
path.google <- ("~/Google Drive/My Drive/")
pathShare <- file.path(path.google, "../Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/")
pathShare2 <- file.path(path.google, "../Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/tables")

# load data ---------------------------------------------------------------

yrs <- read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/year_splines_all_LC.csv")) #individual years
norms <-read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/norms_all_LC_types.csv")) #normals

######################
#loops to find growing season
######################

#find yday 15% of the way between max and min --> start of growing season
#find yday after max where NDVI <= threshold --> end of growing season

for (LC in unique(norms$type)){
  #if (LC=="urban-high"){    #doing urban-high out of loop b/c too wiggly
  #next
  #}
  df <- norms[norms$type==LC,] #subset normal data
  
  LCmax <- max(df$mean) #max mean NDVI
  LCmin <- min(df$mean) #min mean NDVI
  
  maxday <- df[df$mean==LCmax,"yday"] #find yday associated with max/min
  minday <- df[df$mean==LCmin,"yday"]
  
  lower_thresh <- (LCmax-LCmin)*.15 + LCmin #calculate the threshold for growing season
  #upper_thresh <- .95*LCmax
  
  subset_end <- df[df$yday > maxday,] #making two chunks of the dataframe to find threshold
  subset_start <- df[df$yday > minday & df$yday<maxday,]
  
  end_min <- min(subset_end$mean)
  upper_thresh <- (LCmax - end_min)*.15 + end_min
  
  #season_end <- subset_end[which.min(abs(subset_end$mean-upper_thresh)),"yday"] #finding closest data point to threshold
  #season_start <- subset_start[which.min(abs(subset_start$mean-lower_thresh)),"yday"]
  
  #season_end <- max(subset_end$yday[subset_end$mean>=lower_thresh])
  season_end <- max(subset_end$yday[subset_end$mean>=upper_thresh])
  season_start <- min(subset_start$yday[subset_start$mean>=lower_thresh])
  
  growing_season <- df[df$yday >= season_start & df$yday <= season_end,] #define range of growing season
  assign(paste0("grow",LC),growing_season)
}

######################
#saving as separate df
######################

grow_norms <- rbind(growcrop, growforest, `growforest-wet`, growgrassland, `growurban-low`, `growurban-medium`, `growurban-open`, `growurban-high`)
write.csv(grow_norms, file.path(pathShare, "growing_season_norms.csv"), row.names =F)

######################
#loop to make a dataset of growing season for the years dataset
######################

for (LC in unique(yrs$type)){
  df <- yrs[yrs$type==LC,]
  growLC <- grow_norms[grow_norms$type==LC,]
  df <- df[df$yday %in% growLC$yday,]
  LC <- gsub("-","",LC)
  assign(paste0("growyrs",LC),df)
}

growyrs <- rbind(growyrscrop, growyrsforest, growyrsforestwet, growyrsgrassland, growyrsurbanlow, growyrsurbanmedium, growyrsurbanopen, growyrsurbanhigh)
write.csv(growyrs, file.path(pathShare, "growing_season_yrs.csv"), row.names=F)

######################
#reformat table for growing season dates
######################
grow_norms <- grow_norms[grow_norms$type!="forest",]
grow_norms$type[grow_norms$type=="forest-wet"] <- "forest"
grow_norms$date <- as.Date(grow_norms$yday, origin="2022-12-31")
grow_norms$type <- factor(grow_norms$type, levels = c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

grow_dates <- grow_norms[c("type","date")]
grow_dates <- grow_dates %>% group_by(type) %>%
  summarise(
    start = min(date),
    end = max(date)
  )

grow_dates$start <- strftime(grow_dates$start, format="%b %d" )
grow_dates$end <- strftime(grow_dates$end, format="%b %d" )

write.csv(grow_dates, file.path(pathShare2, "appendix_table_2_growing_season_dates_table.csv"), row.names=F)

######################