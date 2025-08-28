library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)

Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")
path.google <- ("~/Google Drive/My Drive/")
pathShare <- file.path(path.google, "../Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/figures")

# load data ------------------------------------------------------

grow_dates <- read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/tables/growing_season_dates_table.csv"))
grow_dates$yday_start <- lubridate::yday(as.Date(grow_dates$start, format="%b %d"))
grow_dates$yday_end <- lubridate::yday(as.Date(grow_dates$end, format="%b %d"))
grow_dates$type <- factor(grow_dates$type, levels = c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

dfHline <- data.frame(graph_type=c("NDVI", "Anoms", "Deriv anoms"), yint = c(NA, 0, 0))
dfHline$graph_type <- factor(dfHline$graph_type, levels=c("NDVI", "Anoms", "Deriv anoms"))

day.labels <- data.frame(Date=seq.Date(as.Date("2023-01-01"), as.Date("2023-12-01"), by="month"))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels$Date))
day.labels
summary(day.labels)

df_full2 <- read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/figure_4_dataframe.csv"))
df_full2$graph_type <- factor(df_full2$graph_type, levels=c("NDVI", "Anoms", "Deriv anoms"))
df_full2$type <- factor(df_full2$type, levels = c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

# figure 4 version 1 ------------------------------------------------------

ggplot(data=df_full2[df_full2$year %in% c(2005, 2012, 2023, "normal"),]) +
  facet_grid(graph_type~type, scales="free_y") +
  geom_rect(data=grow_dates, aes(xmin=yday_start, xmax=yday_end,ymin=-Inf,ymax=Inf), fill="lightblue", alpha= 0.3)+
  geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr, fill=year), alpha=0.5) +
  geom_line(aes(x=yday, y=mean, color=year)) +
  geom_hline(data=dfHline, aes(yintercept=yint), linetype="dashed", color="black")+
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#009E73", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#009E73", "2023"="#CC79A7")) +
  ylab("NDVI")+
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=3)], labels=day.labels$Text[seq(2, 12, by=3)])+
  theme_bw(15)+  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1), legend.position = "bottom")
ggsave("figure_4_years_3X7_panels.png", path = pathShare, height=6, width=12, units="in", dpi = 320)

# figure 4 version 2 ------------------------------------------------------

norms <-read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/norms_all_LC_types.csv")) #normals
norms <- norms[norms$type!="forest",]
norms$type[norms$type=="forest-wet"] <- "forest"
norms$type <- factor(norms$type, levels = c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

p1 <- ggplot(data=df_full2[df_full2$year %in% c(2005, 2012, 2023) & df_full2$graph_type=="NDVI",]) +
  facet_grid(year~type) + ylim(0,1) +
  geom_rect(data=grow_dates, aes(xmin=yday_start, xmax=yday_end,ymin=-Inf,ymax=Inf), fill="lightblue", alpha= 0.3)+
  geom_line(data=norms, aes(x=yday, y=mean,color="normal"))+
  geom_ribbon(data=norms, aes(x=yday, ymin=lwr, ymax=upr,fill="normal"), alpha=0.5) +
  geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr, fill=year), alpha=0.5) +
  geom_line(aes(x=yday, y=mean, color=year)) +
  #geom_hline(data=dfHline, aes(yintercept=yint), linetype="dashed", color="black")+
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#009E73", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#009E73", "2023"="#CC79A7")) +
  ylab("NDVI")+
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=3)], labels=day.labels$Text[seq(2, 12, by=3)])+
  theme_bw(15)+  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1), legend.position = "none")

p2 <- ggplot(data=df_full2[df_full2$year %in% c(2005, 2012, 2023) & df_full2$graph_type=="Anoms",]) +
  facet_grid(year~type) + ylim(-0.3,0.3) +
  geom_rect(data=grow_dates, aes(xmin=yday_start, xmax=yday_end,ymin=-Inf,ymax=Inf), fill="lightblue", alpha= 0.3)+
  geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr, fill=year), alpha=0.5) +
  geom_line(aes(x=yday, y=mean, color=year)) +
  geom_hline(data=dfHline, aes(yintercept=yint), linetype="dashed", color="black")+
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#009E73", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#009E73", "2023"="#CC79A7")) +
  ylab("NDVI Anomaly")+
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=3)], labels=day.labels$Text[seq(2, 12, by=3)])+
  theme_bw(15)+  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1), legend.position = "none")

p3 <- ggplot(data=df_full2[df_full2$year %in% c(2005, 2012, 2023) & df_full2$graph_type=="Deriv anoms",]) +
  facet_grid(year~type) + ylim(-0.01,0.01) +
  geom_rect(data=grow_dates, aes(xmin=yday_start, xmax=yday_end,ymin=-Inf,ymax=Inf), fill="lightblue", alpha= 0.3)+
  geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr, fill=year), alpha=0.5) +
  geom_line(aes(x=yday, y=mean, color=year)) +
  geom_hline(data=dfHline, aes(yintercept=yint), linetype="dashed", color="black")+
  scale_color_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#009E73", "2023"="#CC79A7")) +
  scale_fill_manual(name="year", values=c("normal" = "black", "2005"="#D55E00", "2012"="#009E73", "2023"="#CC79A7")) +
  ylab("NDVI Derivative Anomaly")+
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=3)], labels=day.labels$Text[seq(2, 12, by=3)])+
  theme_bw(15)+  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1), legend.position = "none")

ggpubr::ggarrange(
  p1, p2, p3, # list of plots
  labels = c("a", "b", "c"), # labels
  common.legend = TRUE, # COMMON LEGEND
  legend = "bottom", # legend position
  #align = "hv", # Align them both, horizontal and vertical
  ncol = 1,
  nrow= 3 # number of rows
)
ggsave("alt_figure_4_years_9X7_panels.png", path = pathShare, height=15, width=12, units="in", dpi = 320)

