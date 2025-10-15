# Author: Zhaozhe Chen
# Date: 2025.10.15

# This code is to identify storm events, match them with runoff events, and calculate required P characterization metrics
# Note: This code only considers sites with subdaily USGS data


# -------- Global -----------
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

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
MIT <- 2
# This is threshold used to define rain or not
P_th <- 0

# ------- Main ----------
arrayid <- 1

# Date processing ===================
# Get file name for USGS subdaily P
file_name <- list.files(USGS_subdaily_P_path,full.names = FALSE)[arrayid]
# Get the Site_ID
Site_ID <- str_extract(file_name,"(?<=subdaily_P_).*(?=\\.csv)")
# Read in the file
subdaily_P <- read.csv(paste0(USGS_subdaily_P_path,file_name)) %>%
  # Format Time
  mutate(dateTime = if_else(
    nchar(dateTime)==10,
    paste0(dateTime,"00:00:00"),
    dateTime
  ),
  dateTime = ymd_hms(dateTime,tz="UTC"))

# Separate rainfall events ================
subdaily_P <- subdaily_P %>%
  arrange(dateTime) %>%
  mutate(
    # Label dry obs
    is_rain = Precip_Inst_mm > P_th,
    # Get time difference between each two obs in hours
    time_diff_hr = as.numeric(difftime(dateTime,lag(dateTime),units = "hours")),
    # Remove the first NA in time_diff_hr
    time_diff_hr = replace_na(time_diff_hr,0),
    
    # Define dry groups: each time a dry period starts, increase ID
    dry_start = (!is_rain) & (lag(is_rain,default = TRUE)),
    # These dry groups are consecutive dry obs without rain interruption
    dry_group = if_else(!is_rain,cumsum(dry_start),NA)
) %>%
  group_by(dry_group) %>%
  mutate(
    # Get cumulative dry period duration in hours
    cum_dry_time_hr = if_else(!is_rain,cumsum(time_diff_hr),NA)
    )%>%
  ungroup() %>%
  mutate(
    # Get cumulative dry duration before rain
    dry_time_before_rain = lag(cum_dry_time_hr),
    # Define new rainfall events, if it has been dried for time longer than MIT
    new_event = if_else(
      is_rain & 
        (is.na(lag(is_rain)) | lag(is_rain) == FALSE) &
        (dry_time_before_rain > MIT | is.na(dry_time_before_rain)),
      TRUE,FALSE
    ),
    # Assign rainfall event ID
    Rainfall_ID = if_else(is_rain,cumsum(new_event),NA)
  )
    
    






