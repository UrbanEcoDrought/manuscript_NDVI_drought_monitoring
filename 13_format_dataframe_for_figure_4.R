library(dplyr)
library(tidyverse)

Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")
path.google <- ("~/Google Drive/My Drive/")
pathShare <- file.path(path.google, "../Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data")

# load data ---------------------------------------------------------------

yrs <- read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/year_splines_all_LC.csv"))
yrs <- yrs[yrs$type!="forest",]
yrs$type[yrs$type=="forest-wet"] <- "forest"
yrs$type <- factor(yrs$type, levels = c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

norms <-read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/norms_all_LC_types.csv")) #normals
norms <- norms[norms$type!="forest",]
norms$type[norms$type=="forest-wet"] <- "forest"
norms$type <- factor(norms$type, levels = c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

yrsderivs <- read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/year_splines_derivatives.csv"))
yrsderivs <- yrsderivs[yrsderivs$type!="forest",]
yrsderivs$type[yrsderivs$type=="forest-wet"] <- "forest"
yrsderivs$type <- factor(yrsderivs$type, levels = c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

normsderivs <-read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/norms_derivatives.csv")) #normals
normsderivs <- normsderivs[normsderivs$type!="forest",]
normsderivs$type[normsderivs$type=="forest-wet"] <- "forest"
normsderivs$type <- factor(normsderivs$type, levels = c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

yrs_merge <- yrs %>% inner_join(norms, by= c("type", "yday"), suffix = c("_yrs", "_norm")) 
yrs_merge$mean_anoms <- yrs_merge$mean_yrs - yrs_merge$mean_norm
yrs_merge$upr_anoms <- yrs_merge$upr_yrs - yrs_merge$mean_norm
yrs_merge$lwr_anoms <- yrs_merge$lwr_yrs - yrs_merge$mean_norm

yrsderivs_merge <- yrsderivs %>% inner_join(normsderivs, by= c("type", "yday"), suffix = c("_yrs", "_norm")) 
yrsderivs_merge$mean_anoms <- yrsderivs_merge$mean_yrs - yrsderivs_merge$mean_norm
yrsderivs_merge$upr_anoms <- yrsderivs_merge$upr_yrs - yrsderivs_merge$mean_norm
yrsderivs_merge$lwr_anoms <- yrsderivs_merge$lwr_yrs - yrsderivs_merge$mean_norm
yrsderivs_merge <- yrsderivs_merge[, !names(yrsderivs_merge) %in% c("sig_yrs", "var_yrs", "sig_norm", "var_norm", "mean_yrs", "upr_yrs","lwr_yrs")]

df_full <- yrs_merge %>% inner_join(yrsderivs_merge, by=c("type", "yday", "year"), suffix=c("yrs", "deriv"))
df_full <- df_full %>% pivot_longer(cols=-c("yday","type", "year", "mean_normyrs", "lwr_normyrs", "upr_normyrs", "mean_normderiv", "lwr_normderiv", "upr_normderiv"), names_to = c("pos","graph_type"), names_sep = "_", values_to = "value")
df_full$graph_type[df_full$graph_type=="yrs"] <- "NDVI"
df_full$graph_type[df_full$graph_type=="anomsyrs"] <- "Anoms"
df_full$graph_type[df_full$graph_type=="anomsderiv"] <- "Deriv anoms"
df_full <- df_full %>% pivot_wider(names_from = pos, values_from = value)

dfNormDupe <- df_full[df_full$year==2013 & df_full$graph_type=="NDVI", c("yday", "type", "year",  "graph_type", "mean_normyrs", "lwr_normyrs", "upr_normyrs")]
dfNormDupe$year <- "normal"
names(dfNormDupe) <- c("yday", "type", "year", "graph_type", "mean", "lwr", "upr")

df_full2 <- rbind(df_full[,names(dfNormDupe)], dfNormDupe)
df_full2$graph_type <- factor(df_full2$graph_type, levels=c("NDVI", "Anoms", "Deriv anoms"))

write.csv(df_full2, file.path(pathShare, "figure_4_dataframe.csv"), row.names=F)
