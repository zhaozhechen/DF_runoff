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
  mutate(dateTime = correct_time(dateTime))

# Assign event ID to each unique rainfall event
event_P <- Assign_P_event_ID(subdaily_P,P_th,MIT)

# Keep EOF Q data at this site
eof_site <- eof_df %>%
  filter(Field_Name == Site_ID) %>%
  # Correct time
  mutate(runoff_start = correct_time(storm_start),
         runoff_end = correct_time(storm_end))

# Match P with EOF Q ==================


    
    






