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
# Input path for subdaily USGS P
#USGS_subdaily_P_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/USGS_subdaily_P/"

# Source functions
source("Functions/Data_processing_functions.R")
source("Functions/Plotting_functions.R")

# ------- Main -----------
arrayid <- 8
# Date processing ===================
# Get file name for eof at each site
file_name <- list.files(eof_path,full.names = FALSE)[arrayid]
# Get the Site_ID
Site_ID <- str_extract(file_name,"(?<=subdaily_P_).*(?=\\.csv)")
# Read in eof at this site
eof_df <- read.csv(paste0(eof_path,file_name))

# Calculate secondary metrics
eof_df <- eof_df %>%
  mutate(Q_start = correct_time(Q_start),
         Q_end = correct_time(Q_end),
         # Convert Q unit to mm
         runoff_mm = runoff_in * 25.4,
         # event-scale runoff ratio
         RCe = runoff_mm/P_total)

plot(eof_df$RCe)



