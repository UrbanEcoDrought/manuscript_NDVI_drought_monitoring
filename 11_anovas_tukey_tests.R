library(ggplot2)
library(tidyr)
library(multcompView)
library(stringr)
library(dplyr)
library(lubridate)

Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")
path.google <- ("~/Google Drive/My Drive/")
pathShare <- file.path(path.google, "../Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data")
pathShare2 <- file.path(path.google, "../Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/tables")

# load data ---------------------------------------------------------------

usdmcum <- read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/USDM_cumulative_county_data_2001-2024.csv")) #usdm chicago region cumulative data

grow_norms <- read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/growing_season_norms.csv")) #growing season norms

grow_years <- read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/growing_season_yrs.csv")) #growing season years

# calculate county weights ------------------------------------------------

#https://datahub.cmap.illinois.gov/datasets/a5d89f35ccc54018b690683b49be1ac7_0/explore?location=41.838395%2C-88.115001%2C9.16

# will <- 3917450609.094
# kendall <- 1494102678.262
# cook <- 4472919440.23
# dupage <- 1571636669.879
# kane <- 2454958976.102
# lake <- 2231222407.188
# mchenry <- 2897036640.547

coArea <- c(3917450609.094, 1494102678.262, 4472919440.23, 1571636669.879, 2454958976.102, 2231222407.188, 2897036640.547)
names(coArea) <- c("Will County","Kendall County","Cook County","DuPage County","Kane County","Lake County","McHenry County")
coWeights <- coArea/sum(coArea)

usdmcum$County <- factor(usdmcum$County, levels = c("Will County","Kendall County","Cook County","DuPage County","Kane County","Lake County","McHenry County"))
usdm_county <- data.frame()
for (county in unique(usdmcum$County)){
  usdmInd <- usdmcum[usdmcum$County==county,]
  usdmInd$None <- usdmInd$None*coWeights[county]
  usdmInd$D0 <- usdmInd$D0*coWeights[county]
  usdmInd$D1 <- usdmInd$D1*coWeights[county]
  usdmInd$D2 <- usdmInd$D2*coWeights[county]
  usdmInd$D3 <- usdmInd$D3*coWeights[county]
  usdmInd$D4 <- usdmInd$D4*coWeights[county]
  usdm_county <- rbind(usdm_county, usdmInd)
}

usdmcum <- usdm_county %>% group_by(ValidStart, ValidEnd) %>%
  summarise_at(vars(None, D0, D1, D2, D3, D4),
               sum) %>%
  ungroup()

usdmcum$date <- as.Date(usdmcum$ValidStart)

usdm_table <- usdmcum
usdm_table <- usdm_table %>% mutate(year=year(date))
usdm_table <- usdm_table %>% filter(year %in% c(2005,2012,2023))
usdm_table <- usdm_table[usdm_table$D1 >= 50,]

write.csv(usdm_table, file.path(pathShare2, "USDM_dates_case_study_years.csv"), row.names=F)

# calculate NDVI anomaly --------------------------------------------------

df <- data.frame()

for (LC in unique(grow_years$type)){
  datLC <- grow_years[grow_years$type==LC,]
  
  for (yr in unique(datLC$year)){
    datyr <- datLC[datLC$year==yr,]
    originyr <- yr - 1
    origindate <- paste(originyr,12,31,sep="-")
    datyr$date <- as.Date(datyr$yday, origin=origindate)
    datyr$anomaly <- datyr$mean - (grow_norms[grow_norms$type==LC,])$mean 
    df <- rbind(df,datyr)
  }
}

grow_merge <- merge(x=df, y=usdmcum, by="date", all.x=F, all.y=T)

grow_merge <- grow_merge %>% pivot_longer(cols = c(11:16), names_to = "severity", values_to = "percentage") #combining index columns

#subset data

grow_merge <- grow_merge[!is.na(grow_merge$yday),]
grow_merge <- grow_merge[grow_merge$percentage>50,]
grow_merge <- grow_merge[grow_merge$type!="forest",]
grow_merge$type[grow_merge$type=="forest-wet"] <- "forest"
grow_merge$severity <- factor(grow_merge$severity, levels=c("None", "D0", "D1", "D2", "D3"), ordered=TRUE)

grow_merge <- grow_merge %>% group_by(type, date) %>%
  filter(severity == max(severity)) %>%
  ungroup()

grow_merge$type <- factor(grow_merge$type, levels = c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

write.csv(grow_merge, file.path(pathShare, "growing_season_USDM_data_boxplot.csv"), row.names=F)

# anovas by LC type ------------------------------------------------------------------
letter_list <- data.frame()

for (LC in unique(grow_merge$type)){
  anovaLC <- aov(anomaly ~ severity, data=grow_merge[grow_merge$type==LC,])
  tukeyLC <- TukeyHSD(anovaLC, conf.level = 0.95)
  LCletters <- multcompLetters4(anovaLC, tukeyLC)
  LCletters <- as.data.frame.list(LCletters$severity)
  LCletters <- rownames_to_column(LCletters, "severity")
  LCletters$severity <- factor(LCletters$severity, levels = c("None", "D0", "D1", "D2", "D3"), ordered=TRUE)
  LCletters$type <- LC
  LCletters <- LCletters[, names(LCletters) %in% c("severity", "Letters", "type")]
  
  letter_list <- rbind(letter_list, LCletters)
  
}

# anovas by USDM category -------------------------------------------------
grow_merge2 <- grow_merge
grow_merge2$type <- gsub("-", "", grow_merge2$type)

cat_letter_list <- data.frame()

for (category in unique(grow_merge$severity)){
  anova_category <- aov(anomaly ~ type -1, data=grow_merge2[grow_merge2$severity==category,])
  tukey_category <- TukeyHSD(anova_category, conf.level = 0.95)
  cat_letters <- multcompLetters4(anova_category, tukey_category)
  cat_letters <- as.data.frame.list(cat_letters$type)
  cat_letters <- cat_letters["Letters"]
  cat_letters$severity <- category
  cat_letters <- rownames_to_column(cat_letters, "type")
  
  cat_letter_list <- rbind(cat_letter_list, cat_letters)
}

cat_letter_list$Letters <- toupper(cat_letter_list$Letters)
cat_letter_list$type <- str_replace(cat_letter_list$type, "^urban", "urban-")

# mean anomaly & SD -------------------------------------------------------

grow_sum <- group_by(grow_merge, type, severity) %>% 
  summarise(mean_anom=mean(anomaly),sd=sd(anomaly))

grow_sum <- inner_join(grow_sum, letter_list, by=c("type", "severity"))
grow_sum <- grow_sum %>% rename(tukey_LC_type = Letters)

grow_sum <- grow_sum %>% inner_join(cat_letter_list, by=c("type", "severity"))
grow_sum <- grow_sum %>% rename(tukey_USDM_category = Letters)

grow_sum$type <- factor(grow_sum$type, levels = c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

write.csv(grow_sum, file.path(pathShare2, "boxplot_anomalies_tukey_table.csv"), row.names=F)
