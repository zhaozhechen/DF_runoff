# Author: Zhaozhe Chen
# Date: 2025.10.15

# This code calculates metrics related to P-Q relationships

# -------- Global ---------
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# Data paths =====
# Cleaned EOF dataframe
eof_df <- read.csv("00_Data/Processed_data/Cleaned_data/DF_EOF_cleaned.csv")
# PRISM ppt
ppt_df <- read.csv("00_Data/Processed_data/DF_PRISM_ppt.csv")
# DF site info
site_info <- read.csv("00_Data/Processed_data/Cleaned_data/DF_site_info_cleaned.csv")

# Source functions
source("Functions/Data_processing_functions.R")
source("Functions/Plotting_functions.R")



# ------- Main --------------
# Sites to process
Site_ls <- site_info$Field_Name

arrayid <- 1

Site_ID <- Site_ls[arrayid]
# Get eof data at this site
eof_site <- eof_df %>%
  filter(Field_Name == Site_ID)

