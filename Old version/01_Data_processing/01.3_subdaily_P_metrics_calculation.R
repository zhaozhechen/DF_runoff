# Author: Zhaozhe Chen
# Date: 2025.10.16

# This code is to identify storm events, match them with runoff events, and calculate required P characterization metrics
# Note: This code only considers sites with subdaily USGS data

# -------- Global -----------
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(slider)

# Data paths =====
# Cleaned EOF dataframe
eof_df <- read.csv("00_Data/Processed_data/Cleaned_data/DF_EOF_cleaned.csv")
# PRISM ppt
#ppt_df <- read.csv("00_Data/Processed_data/DF_PRISM_ppt.csv")
# DF site info
site_info <- read.csv("00_Data/Processed_data/Cleaned_data/DF_site_info_cleaned.csv")
# Input path for subdaily USGS P
USGS_subdaily_P_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/USGS_subdaily_P/"

# Source functions
source("Functions/Data_processing_functions.R")
source("Functions/Plotting_functions.R")

# Minimum inter-event time (MIT) to separate rainfall event
# Initial test of 2 hours, align with EOF definition of runoff events
#MIT <- 2
# This is threshold used to define rain or not
P_th <- 0

# This is output path for event P at each site
EventP_out_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/USGS_subdaily_Pevent/"

# This is output path for P metrics calculated at each site
EOF_out_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/EOF_subdaily_P_metrics/"

# ------- Main ----------
# Test different MIT values
for(MIT in c(2,6,12,24)){
  # Loop over the 16 sites
  for(arrayid in 1:16){
    # Date processing ===================
    # Get file name for USGS subdaily P
    file_name <- list.files(USGS_subdaily_P_path,full.names = FALSE)[arrayid]
    # Get the Site_ID
    Site_ID <- str_extract(file_name,"(?<=subdaily_P_).*(?=\\.csv)")
    
    # Get the start time at this site
    eof_start_time <- site_info$Approximate_Start_Date[site_info$Field_Name == Site_ID]
    eof_start_time <- ymd_hms(paste(mdy(eof_start_time),"00:00:00"),tz="UTC")
    # Keep EOF data only at this site
    eof_site <- eof_df %>%
      filter(Field_Name == Site_ID) %>%
      # Correct time, which should be the time of Q
      mutate(Q_start = correct_time(storm_start),
             Q_end = correct_time(storm_end))
    # End time of eof runoff
    eof_end_time <- tail(eof_site$Q_end,1)
    
    # Read in the USGS subdaily P file
    subdaily_P <- read.csv(paste0(USGS_subdaily_P_path,file_name)) %>%
      mutate(dateTime = correct_time(dateTime))
    # Only keep data after the start time of this eof site and before the end of the last Q
    #filter(dateTime >= eof_start_time & dateTime <= eof_end_time)
    
    # Assign event ID to each unique rainfall event
    event_P <- Assign_P_event_ID(subdaily_P,P_th,MIT)
    # Output this event P
    write.csv(event_P,paste0(EventP_out_path,"USGS_subdaily_Pevent_",MIT,"hr_",Site_ID,".csv"))
    
    # Get P event time range
    P_event_ranges <- event_P %>%
      filter(!is.na(P_event_ID)) %>%
      group_by(P_event_ID) %>%
      summarize(
        P_start = min(dateTime,na.rm=TRUE),
        P_end = max(dateTime,na.rm=TRUE),
        # Duration in hours
        P_duration = difftime(P_end,P_start,units = "hours"),
        P_duration = round(as.numeric(P_duration),2)
      ) %>%
      # Filter out events that lasts for at least 15 min
      filter(P_duration > 0.25)
    
    # Match P with EOF Q and calculate P-related metrics ==================
    eof_site$Lag_time <- NA
    eof_site$P_total <- NA
    eof_site$P_duration <- NA
    eof_site$Ievent <- NA
    eof_site$I30_max <- NA
    eof_site$I60_max <- NA
    eof_site$API5 <- NA
    
    # Loop over each event in eof_site
    for(i in 1:nrow(eof_site)){
      # start and end time recorded in eof_site, representing Q time
      Q_start <- eof_site$Q_start[i]
      Q_end <- eof_site$Q_end[i]
      
      # Find overlapping P events
      matched <- P_event_ranges %>%
        filter(P_start <= Q_end & P_end >= Q_start)
      
      # If there is no overlapping P event, find the last P event before the start of Q
      if(nrow(matched) == 0){
        matched <- P_event_ranges %>%
          filter(P_start <= Q_start) %>%
          tail(1)
      }
      
      # Track the earliest P start time and latest P end time
      P_start_event <- min(matched$P_start)
      # Only keep P that happens before the end of the Q event
      P_end_event <- min(max(matched$P_end),Q_end)
      
      # Response time, the time takes for Q to start after the start of rainfall event (hours)
      Lag_time <- round(as.numeric(difftime(Q_start,P_start_event,units = "hours")),2)
      
      # If this lag time is negative, need to match with an earlier P_event
      if(Lag_time < 0){
        # Find latest P event that happens before the Q event
        matched <- P_event_ranges %>%
          filter(P_start <= Q_start) %>%
          tail(1)
        
        # Update start time
        P_start_event <- matched$P_start
        # But keep the original end time
      }
      
      # Update Lag time
      Lag_time <- round(as.numeric(difftime(Q_start,P_start_event,units = "hours")),2)
      
      # Duration of this P event (hours)
      P_duration <- difftime(P_end_event,P_start_event,units = "hours")
      P_duration <- round(as.numeric(P_duration),2)
      
      # Extract all P time series during this P event
      P_TS <- event_P %>%
        filter(dateTime >= P_start_event & dateTime <= P_end_event)
      
      # Total P during the event (mm)
      P_total <- sum(P_TS$Precip_Inst_mm)
      
      # Mean event intensity (mm/hr)
      Ievent <- P_total/P_duration
      
      # Resample to regular 1 minute intervals, filling gaps with 0
      P_regular <- data.frame(
        dateTime = seq(P_start_event,P_end_event,by="1 min")
      ) %>%
        left_join(P_TS,by="dateTime") %>%
        mutate(Precip_Inst_mm = replace_na(Precip_Inst_mm,0))
      
      # Rolling 30-min and 60-min intensity (mm/hr)
      P_regular <- P_regular %>%
        mutate(
          I30 = slide_dbl(Precip_Inst_mm,sum,.before = 29,.complete = FALSE) * 2,
          I60 = slide_dbl(Precip_Inst_mm,sum,.before = 59,.complete = FALSE)
        )
      
      # Maximum I30
      I30_max <- max(P_regular$I30,na.rm=TRUE)
      # Maximum I60
      I60_max <- max(P_regular$I60,na.rm=TRUE)
      
      # Extract all P time series five days before this Q event
      P_TS_5d <- event_P %>%
        filter(dateTime >= (Q_start - days(5)) & dateTime < Q_start)
      # Get total P 5 days prior (Antecedent P index - API)
      API5 <- sum(P_TS_5d$Precip_Inst_mm,na.rm=TRUE)
      
      # Add these metrics to eof_site
      eof_site$Lag_time[i] <- Lag_time
      eof_site$P_total[i] <- P_total
      eof_site$P_duration[i] <- P_duration
      eof_site$Ievent[i] <- Ievent
      eof_site$I30_max[i] <- I30_max
      eof_site$I60_max[i] <- I60_max
      eof_site$API5[i] <- API5
    }
    
    # Clean output if I30_max or I60_max is -Inf
    eof_site <- eof_site %>%
      mutate(
        I30_max = if_else(I30_max < 0,NA,I30_max),
        I60_max = if_else(I60_max < 0,NA,I60_max),
        # Convert Q unit to mm
        runoff_mm = runoff_in*25.4,
        # event-scale runoff ratio
        RCe = runoff_mm/P_total
      ) %>%
      select(-X)
    
    # Output EOF data with matched P metrics at this site
    write.csv(eof_site,paste0(EOF_out_path,"EOF_subdaily_P_metrics_",MIT,"hr_",Site_ID,".csv"))
    
    message(arrayid)
  }
  message("MIT value of ",MIT,"hr done")
}










