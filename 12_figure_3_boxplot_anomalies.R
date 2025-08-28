library(ggplot2)
library(tidyr)
library(cowplot)

Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")
path.google <- ("~/Google Drive/My Drive/")
pathShare <- file.path(path.google, "../Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/figures")

# load data ---------------------------------------------------------------

grow_merge <-read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/data/growing_season_USDM_data_boxplot.csv"))
grow_merge$type <- factor(grow_merge$type, levels = c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))
grow_merge$severity <- factor(grow_merge$severity, levels=c("None", "D0", "D1", "D2", "D3"))

grow_sum <-read.csv(file.path(google.drive, "Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/tables/boxplot_anomalies_tukey_table.csv"))
grow_sum$type <- factor(grow_sum$type, levels = c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

# boxplot -----------------------------------------------------------------

p1 <- ggplot()+ #boxplots by drought category for each LC type
  geom_boxplot(data=grow_merge,aes(x=severity, y=anomaly, fill=severity)) + ylab("NDVI Anomaly") + xlab(NULL) +
  scale_fill_manual(name="USDM Category", values=c("None"="gray50", "D0"="#ffff00", "D1"="#fbd380","D2"="#ffaa03", "D3"="#e60001"))+
  geom_text(data=grow_sum, aes(label=tukey_LC_type, x=severity,y=mean_anom+sd),size = 3, vjust=-2, hjust =-1)+
  geom_text(data=grow_sum, aes(label=tukey_USDM_category, x=severity,y=mean_anom-sd),size = 3, vjust=2, hjust =-1)+
  facet_wrap(~type)+
  geom_hline(yintercept=0, linetype="dashed")+
  #labs(caption = "lowercase = effect of drought severity on NDVI anomalies within a given land cover class \nuppercase = effect of land cover type on NDVI anomalies within a given drought category") +
  ylim(-0.2,0.2) + theme_bw(15) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),plot.caption.position="plot",
                                        plot.caption = element_text(hjust=0,vjust=0.5),plot.margin = margin(5.5,5.5,20,5.5,"pt"), legend.position = "bottom")

#ggsave("NDVI_anoms_boxplot_with_letters_wet-forest.png", path = pathShare, height=6, width=12, units="in", dpi = 320)

# barplot -----------------------------------------------------------------

grow_merge_unique <- grow_merge %>% distinct(severity, date, .keep_all = TRUE)

# ggplot(data=grow_merge_unique, aes(x=as.factor(severity), fill=as.factor(severity)))+
#   geom_bar() +
#   scale_fill_manual(name="Category", values=c("None"="gray50", "D0"="yellow", "D1"="burlywood","D2"="darkorange", "D3"="red"))

p2 <- ggplot(data=grow_merge_unique[grow_merge_unique$severity!="None",], aes(x=as.factor(severity), fill=as.factor(severity)))+
  geom_bar() + xlab("USDM Category") + ylim(0,125)+
  scale_fill_manual(name="USDM Category", values=c("D0"="#ffff00", "D1"="#fbd380","D2"="#ffaa03", "D3"="#e60001"))+
  theme_bw(15) + theme(legend.position = "none")

# joint figure ------------------------------------------------------------

ggpubr::ggarrange(
  p1, p2,# list of plots
  labels = c("a", "b"), # labels
  common.legend = TRUE, # COMMON LEGEND
  legend = "bottom", # legend position
  #align = "hv", # Align them both, horizontal and vertical
  ncol = 1,
  nrow= 2, # number of rows
  heights = c(2,1)
)

ggsave("figure_3_NDVI_anoms_boxplot.png", path = pathShare, height=8, width=12, units="in", dpi = 320)


#plot_grid(p1,p2, ncol=1, rel_heights =c(2,1), labels = "auto")
