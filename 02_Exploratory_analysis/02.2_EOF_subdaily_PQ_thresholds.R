# Author: Zhaozhe Chen
# Date: 2025.10.17

# ---------- Global ---------
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(segmented)

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

# Output path for figures
Output_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/EOF_subdaily_bp_figures/"

# Minimum inter-event time (MIT) to separate rainfall event
MIT <-2

# ---- Main ------------
arrayid <- 1

for(arrayid in 1:16){
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
                                  tz = attr(Q_start, "tzone"))) %>%
    # Only keep Non-frozen soil
    filter(frozen == "Non-Frozen")
  
  # Plot response variables against explanatory variables and check for break point
  RCevsP_total <- BP_plot(eof_df,"RCe","P_total")[[1]]+
    ggtitle(paste(Site_ID,"MIT =",MIT))
  RCevsP_duration <- BP_plot(eof_df,"RCe","P_duration")[[1]]
  RCevsIevent <- BP_plot(eof_df,"RCe","Ievent")[[1]]
  RCevsI30max <- BP_plot(eof_df,"RCe","I30_max")[[1]]
  RCevsI60max <- BP_plot(eof_df,"RCe","I60_max")[[1]]
  RCevsAPI5 <- BP_plot(eof_df,"RCe","API5")[[1]]
  
  Lag_timevsP_total <- BP_plot(eof_df,"Lag_time","P_total")[[1]]
  Lag_timevsP_duration <- BP_plot(eof_df,"Lag_time","P_duration")[[1]]
  Lag_timevsIevent <- BP_plot(eof_df,"Lag_time","Ievent")[[1]]
  Lag_timevsI30max <- BP_plot(eof_df,"Lag_time","I30_max")[[1]]
  Lag_timevsI60max <- BP_plot(eof_df,"Lag_time","I60_max")[[1]]
  Lag_timevsAPI5 <- BP_plot(eof_df,"Lag_time","API5")[[1]]
  
  # Combine all these plots
  g_all <- plot_grid(RCevsP_total,RCevsP_duration,RCevsIevent,
                     RCevsI30max,RCevsI60max,RCevsAPI5,
                     Lag_timevsP_total,Lag_timevsP_duration,Lag_timevsIevent,
                     Lag_timevsI30max,Lag_timevsI60max,Lag_timevsAPI5,
                     nrow=4,align = "hv",labels = "auto")
  # Output these figures at each site
  print_g(g_all,paste0(Site_ID,"_MIT"),9,12)
  message(arrayid)
}





