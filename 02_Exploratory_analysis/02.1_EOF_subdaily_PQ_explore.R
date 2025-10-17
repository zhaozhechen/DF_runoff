# Author: Zhaozhe Chen
# Date: 2025.10.16

# This code is to explore P-Q relationships using subdaily USGS P data


# ---------- Global ---------
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# EOF data with subdaily P metrics
eof_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/EOF_subdaily_P_metrics/"
# DF site info
site_info <- read.csv("00_Data/Processed_data/Cleaned_data/DF_site_info_cleaned.csv")
# USGS Subdaily P events 
Pevent_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/USGS_subdaily_Pevent/"

# Source functions
source("Functions/Data_processing_functions.R")
source("Functions/Plotting_functions.R")

# Sites to test (sites with subdaily USGS P)
site_ls <- c("AO1","AO3","AR1","AR2","DK1A","KD1","KD2","MA1A","MT1","MT2","RE1","RE5","RS1","RS2","SH1A","WF1")

my_color <- brewer.pal(5,"Set2")

# ------- Main -----------
# Minimum inter-event time (MIT) to separate rainfall event
MIT <- 2
arrayid <- 1

# Date processing ===================
Site_ID <- site_ls[arrayid]

# Read in eof at this site
eof_df <- read.csv(paste0(eof_path,"EOF_subdaily_P_metrics_",MIT,"hr_",Site_ID,".csv"))

# Format eof_df
eof_df <- eof_df %>%
  mutate(Q_start = correct_time(Q_start),
         Q_end = correct_time(Q_end),
         # Get middle time of them
         Q_midtime = as.POSIXct((as.numeric(Q_start) + as.numeric(Q_end)) / 2,
                                origin = "1970-01-01",
                                tz = attr(Q_start, "tzone"))
         )

# Read in subdaily P event at this site
Pe_df <- read.csv(paste0(Pevent_path,"USGS_subdaily_Pevent_",MIT,"hr_",Site_ID,".csv"))

# Format Pe_df and group by event
Pe_df <- Pe_df %>%
  filter(!is.na(P_event_ID))%>%
  mutate(dateTime = correct_time(dateTime)) %>%
  group_by(P_event_ID) %>%
  summarize(P_start = first(dateTime),
            P_end = last(dateTime),
            P_total_mm = sum(Precip_Inst_mm)) %>%
  ungroup() %>%
  # Make a label for easier color separation
  mutate(P_event_label = P_event_ID %% 2,
         year = year(P_start))

# Making plots ====================
# TS plot of P and Q
PQ_TS <- ggplot()+
  geom_bar(data=Pe_df,
           aes(x=P_start,y=P_total_mm,color="P",fill="P"),
           stat = "identity")+
  geom_bar(data = eof_df,
           aes(x=Q_midtime,y=-runoff_mm,color="Q",fill="Q"),
           stat = "identity")+
  scale_color_manual(labels = c("P","Q"),
                     values = my_color[c(3,2)])+
  scale_fill_manual(labels = c("P","Q"),
                     values = my_color[c(3,2)])+
  geom_hline(yintercept = 0)+
  guides(color="none")+
  labs(x = "",y="P/Q (mm)",fill="")+
  my_theme2+
  theme(legend.position = c(0.2,0.95),
        legend.background = element_blank())+
  ggtitle(paste(Site_ID,"(MIT =",MIT,"hr)"))

# event-scale Runoff Coefficient
RCe_TS <- ggplot(data=eof_df,aes(x=X,y=RCe,color=frozen))+
  geom_point(size=2)+
  scale_color_manual(labels=c("Frozen","Non-Frozen"),
                     values = my_color[c(1,4)])+
  my_theme2+
  theme(legend.position = c(0.2,0.95),
        legend.background = element_blank())+
  labs(x="",y="RCe",color="")

# EOF P Q metrics distributions
Lag_time_hist <- Hist_plot(eof_df,"Lag_time","Lag time (hr)")+
  theme(legend.position = c(0.7,0.85),
        legend.background = element_blank())

P_total_hist <- Hist_plot(eof_df,"P_total","Event total P (mm)")

P_duration_hist <- Hist_plot(eof_df,"P_duration","P duration (hr)")

Ievent_hist <- Hist_plot(eof_df,"Ievent","Event intensity (mm/hr)")

I30_hist <- Hist_plot(eof_df,"I30_max","I30 (mm/hr)")

I60_hist <- Hist_plot(eof_df,"I60_max","I60 (mm/hr)")

API5_hist <- Hist_plot(eof_df,"API5","API5 (mm)")

RCe_hist <- Hist_plot(eof_df,"RCe","RCe (mm/mm)")

# Put these histograms together
g_hist <- plot_grid(Lag_time_hist,P_total_hist,P_duration_hist,Ievent_hist,
               I30_hist,I60_hist,API5_hist,RCe_hist,
               nrow=2,align = "hv")

















